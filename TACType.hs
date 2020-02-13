module TACType where

class SymEntryCompatible a where
  getSymID :: a -> String

data (SymEntryCompatible a) => ThreeAddressCode a b = TACC
  { tacOperand :: Operation,
    tacLvalue  :: Maybe (Operand a b),
    tacRvalue1 :: Maybe (Operand a b),
    tacRvalue2 :: Maybe (Operand a b)
  }
  deriving (Eq)

instance (SymEntryCompatible a, Show a, Show b) => Show (ThreeAddressCode a b) where
  show (TACC Assign (Just x) (Just y) _)          = show x ++ " := " ++ show y
  show (TACC Add (Just x) (Just y) (Just z))      = show x ++ " := " ++ show y ++ " + " ++ show z
  show (TACC Minus (Just x) (Just y) Nothing)     = show x ++ " := -" ++ show y 
  show (TACC Sub (Just x) (Just y) (Just z))      = show x ++ " := " ++ show y ++ " - " ++ show z
  show (TACC Mult (Just x) (Just y) (Just z))     = show x ++ " := " ++ show y ++ " * " ++ show z
  show (TACC Div (Just x) (Just y) (Just z))      = show x ++ " := " ++ show y ++ " / " ++ show z
  show (TACC Mod (Just x) (Just y) (Just z))      = show x ++ " := " ++ show y ++ " % " ++ show z
  show (TACC (Cast _ toType) (Just x) (Just y) _) = show x ++ " := " ++ toType ++ "(" ++ show y ++ ")"
  show (TACC Not (Just x) (Just y) _)             = show x ++ " := ~" ++ show y
  show (TACC And (Just x) (Just y) (Just z))      = show x ++ " := " ++ show y ++ " && " ++ show z
  show (TACC Or (Just x) (Just y) (Just z))       = show x ++ " := " ++ show y ++ " || " ++ show z
  show (TACC GoTo Nothing Nothing (Just label))   = "goto " ++ show label
  show (TACC GoTo Nothing Nothing Nothing)        = "goto __"
  show (TACC If Nothing (Just b) (Just label))    = "if " ++ show b ++ " then goto " ++ show label
  show (TACC If Nothing (Just b) Nothing)         = "if " ++ show b ++ " then goto __"
  show (TACC Eq (Just x) (Just y) (Just label))   = "if " ++ show x ++ " = " ++ show y ++ " then goto " ++ show label
  show (TACC Neq (Just x) (Just y) (Just label))   = "if " ++ show x ++ " != " ++ show y ++ " then goto " ++ show label
  show (TACC Lt (Just x) (Just y) (Just label))   = "if " ++ show x ++ " < " ++ show y ++ " then goto " ++ show label
  show (TACC Gt (Just x) (Just y) (Just label))   = "if " ++ show x ++ " > " ++ show y ++ " then goto " ++ show label
  show (TACC Lte (Just x) (Just y) (Just label))   = "if " ++ show x ++ " <= " ++ show y ++ " then goto " ++ show label
  show (TACC Gte (Just x) (Just y) (Just label))   = "if " ++ show x ++ " >= " ++ show y ++ " then goto " ++ show label
  show (TACC NewLabel Nothing (Just label) Nothing)   = show label ++ ":"
  show tac = show (tacLvalue tac) ++ " := " ++ show (tacRvalue1 tac) ++ " (?) " ++ show (tacRvalue2 tac)


data (SymEntryCompatible a) => Operand a b = 
  Variable a           | 
  Constant (String, b) | 
  Label Int
  deriving (Eq)


instance (SymEntryCompatible a, Show a, Show b) => Show (Operand a b) where
  show (Variable x) = show x
  show (Constant c) = fst c
  show (Label l)    = show l


data Operation =
    Assign            |
    -- Arithmetic
    -- | Addition
    Add               |
    -- | Substraction
    Sub               |
    -- | Unary minus
    Minus             |
    -- | Multiplication
    Mult              |
    -- | Division
    Div               |
    -- | Modulus
    Mod               |

    -- Logical
    -- | Logical and
    And               |
    -- | Logical or
    Or                |
    -- | Logical not
    Not               |

    -- Comparators
    -- | Greater than
    Gt                |
    -- | Greater than or equal
    Gte               |
    -- | Less than
    Lt                |
    -- | Less than or equal
    Lte               |
    -- | Equal
    Eq                |
    -- | Not equal
    Neq               |

    -- Jumping
    -- | goto <label>
    GoTo              |
    -- | if <var> goto <label>
    If                |
    -- | if ~<var> goto <label>
    IfFalse           |
    -- | New label
    NewLabel          |

    -- Calling functions
    -- | Define a parameter
    Param             |
    -- | Call function
    Call              |

    -- Array operators
    -- | x=y[i]
    Get               |
    -- | x[i]=y
    Set               |
    -- | x:= 5:y
    Anexo             |
    -- | x:= y::z
    Concat            |

    -- Pointer operations
    -- | x=&y
    Ref               |
    -- | x=*y
    Deref             |

    Cast String String

    deriving (Eq, Show)
