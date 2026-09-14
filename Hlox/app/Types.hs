module Types where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict       as SM

data TokenType
  = -- Single-character tokens
    LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE
  | COMMA      | DOT         | MINUS      | PLUS
  | SEMICOLON  | SLASH       | STAR

  -- Double-character tokens
  | BANG       | BANG_EQUAL    | EQUAL | EQUAL_EQUAL
  | GREATER    | GREATER_EQUAL | LESS  | LESS_EQUAL
  | PLUS_EQUAL | MINUS_EQUAL

  -- Literals
  | IDENTIFIER | STRING | NUMBER

  -- keywords
  | IF     | ELSE     | FOR     | WHILE
  | BREAK  | CONTINUE | RETURN
  | AND    | OR       | TRUE    | FALSE
  | VAR    | FUN      | CLASS
  | THIS   | SUPER
  | NIL
  | PRINT

  | EOF
  deriving (Show, Eq)

data LoxValue
  = L_NUMBER Double
  | L_STRING BS.ByteString
  | L_BOOL   Bool
  | L_NIL
  | L_CALL  LoxCallable
  | L_FUN  LoxFunction
  | L_CLASS LoxClass
  | L_INSTANCE LoxInstance

instance Show LoxValue where
  show (L_NUMBER d)      = show d
  show (L_STRING s)      = show s
  show (L_BOOL b)        = show b
  show L_NIL             = "nil"
  show (L_FUN _)         = "<fn>"
  show (L_CALL _)        = "<native fn>"
  show (L_CLASS c)       = "<class " ++ BS.unpack (lc_name c) ++ ">"
  show (L_INSTANCE inst) =
    "<instance " ++ BS.unpack (lc_name (li_class inst)) ++ ">"

instance Eq LoxValue where
  L_NIL == L_NIL = True
  L_BOOL a == L_BOOL b = a == b
  L_STRING a == L_STRING b = a == b

  L_NUMBER a == L_NUMBER b
    | isNaN a && isNaN b = True
    | otherwise          = a == b

  _ == _ = False

data Token = Token
  { t_type     :: TokenType
  , t_lexeme   :: BS.ByteString
  , t_loxValue :: Maybe LoxValue
  , t_line     :: Int
  }
  deriving (Show)

data Loc = Loc
  { l_start :: Int
  , l_curr  :: Int
  , l_line  :: Int
  }
  deriving (Show)

data Scanner = Scanner
  { s_source :: BS.ByteString
  , s_tokens :: [Token]
  , s_loc    :: Loc
  , s_errors :: [BS.ByteString]
  }
  deriving (Show)

data Env = Env
  { e_map        :: SM.Map BS.ByteString LoxValue
  , e_enclosing  :: Maybe Env
  , e_loop_depth :: Int
  }
  deriving (Show)

data Expr
  = E_ASSIGN   Token Expr          -- name value
  | E_BINARY   Expr Token Expr     -- left operator right
  | E_LITERAL  (Maybe LoxValue)    -- value
  | E_LOGICAL  Expr Token Expr     -- left operator right
  | E_UNARY    Token Expr          -- operator right
  | E_CALL     Expr Token [Expr]   -- callee paren args
  | E_GET      Expr Token          -- object name
  | E_SET      Expr Token Expr     -- object name value 
  | E_SUPER    Token Token         -- keyword method
  | E_THIS     Token               -- keyword
  | E_GROUPING Expr                -- expression
  | E_VARIABLE Token               -- name
  deriving (Show)

data Stmt
  = S_BLOCK      [Stmt]                                       -- statements
  | S_CLASS      Token (Maybe Expr) [Stmt]                    -- name, superclass, methods
  | S_EXPRESSION Expr                                         -- expression
  | S_FUNCTION   Token [Token] [Stmt]                         -- name params body
  | S_IF         Expr Stmt (Maybe Stmt)                       -- condition thenBranch elseBranch
  | S_PRINT      Expr                                         -- expression
  | S_RETURN     Token (Maybe Expr)                           -- keyword value
  | S_VAR        Token (Maybe Expr)                           -- name initializer
  | S_WHILE      Expr Stmt                                    -- condition body
  | S_FOR        (Maybe Stmt) (Maybe Expr) (Maybe Expr) Stmt  -- init cond inc body
  | S_BREAK
  | S_CONTINUE
  deriving (Show)

data LoxClass = LoxClass
  { lc_name    :: BS.ByteString
  , lc_super   :: Maybe LoxClass
  , lc_methods :: SM.Map BS.ByteString LoxFunction
  }

instance Show LoxClass where
  show klass = BS.unpack (lc_name klass)

data LoxInstance = LoxInstance
  { li_class  :: LoxClass
  , li_fields :: SM.Map BS.ByteString LoxValue
  }

instance Show LoxInstance where
  show inst = BS.unpack (lc_name (li_class inst)) ++ " instance"

data Parser = Parser [Token] Token Token   -- Parser Tokens Prev Curr
  deriving (Show)

data ExecResult
  = ER_NORMAL   Interpreter
  | ER_BREAK    Interpreter
  | ER_CONTINUE Interpreter
  | ER_RETURN   LoxValue Interpreter

data Interpreter = Interpreter
  { i_globals     :: Env
  , i_environment :: Env
  }
  deriving (Show)

data LoxCallable = LoxCallable
  { lc_arity :: Int
  , lc_call  :: Interpreter -> Token -> [LoxValue] -> EitherListBsLoxValueInterp
  }

data LoxFunction = LoxFunction
  { lf_name        :: Token
  , lf_params      :: [Token]
  , lf_body        :: [Stmt]
  , lf_closure     :: Env
  , lf_initializer :: Bool
  }
  deriving (Show)

-- Now define your Either types
type EitherEnv = Either BS.ByteString Env
type EitherLoxValue = Either BS.ByteString LoxValue
type EitherParserTok = Either BS.ByteString (Parser, Token)
type EitherStmtParser = Either BS.ByteString (Stmt, Parser)
type EitherExprParser = Either BS.ByteString (Expr, Parser)
type EitherLoxCallable = Either BS.ByteString LoxCallable
type EitherListBsExecRes = Either BS.ByteString ([BS.ByteString], ExecResult)
type EitherListStmtParser = Either BS.ByteString ([Stmt], Parser)
type EitherListBsLoxValueInterp = Either BS.ByteString ([BS.ByteString], LoxValue, Interpreter)

