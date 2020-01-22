module TACType where

class SymEntryCompatible a where
  getSymID :: a -> String


data (SymEntryCompatible a) => ThreeAddressCode a b = ThreeAddressCode
  { tacOperand :: Operation,
    tacLvalue  :: Maybe (Operand a b),
    tacRvalue1 :: Maybe (Operand a b),
    tacRvalue2 :: Maybe (Operand a b)
  }

data (SymEntryCompatible a) => Operand a b = Variable a | Constant b

data Operation =
  Add             | -- Addition (2 operands)
  Substract       | -- Substraciton (2 operands)
  Multiply        | -- Multiplication (2 operands)
  Divide          | -- Division (2 operands)
  IntDivide       | -- Integer Division (2 operands)
  Modulo          | -- Modulus / Remainder (2 operands)
  And             | -- And / Conjuction (2 operands)
  Or              | -- Or / Disjunction (2 operands)
  Gt              | -- Greater Than (2 operands)
  Gte             | -- Greater Than or Equal To (2 operands)
  Lt              | -- Less Than (2 operands)
  Lte             | -- Less Than or Equal To (2 operands)
  Equal           | -- Equality (2 operands)
  NotEqual        | -- Inequality (2 operands)
  Concat          | -- Concatenation (2 operands)
  AnexoConcat
  deriving (Eq)
