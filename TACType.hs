module TACType where





class SymEntryCompatible where
  getSymID :: a -> String


data ThreeAddressCode = ThreeAddressCode
  { tacOperand :: Operation,
    tacLvalue  :: Maybe Operand,
    tacRvalue1 :: Maybe Operand,
    tacRvalue2 :: Maybe Operand
  }


data Operand a = Variable a | Constant a


data Operation =
  Suma            |
  Resta           |
  Multiplicacion  |
  Division        |
  DivisionEntera  |
  Y               |
  O               |
  Modulo          |
  Mayor           |
  MayorQue        |
  Menor           |
  MenorQue        |
  Igual           |
  NoIgual         |
  Concatenacion   |
  AnexoConcat
  deriving (Eq)
