module Sols where
import Data.Monoid

class Monoid a => Group a where
  ginverse :: a -> a


instance Num a => Group (Sum a) where
  ginverse = (* (-1))


-- Q2

data Turn = Nought | OneTwenty | TwoForty -- angles in degrees
  deriving (Eq, Enum, Show)
data Axis = X | Y | Z -- X top vertex, Y bottom right vertex, Z bottom left vertex
  deriving (Eq, Enum, Show)
data EqTriSym = Rotn Turn -- rotations
              | Refl Axis -- reflections
  deriving Show


(|>) :: EqTriSym -> EqTriSym -> EqTriSym
Rotn a |> Rotn b = Rotn (a `turnTurn` b)
  where
    turnTurn a b = toEnum ((fromEnum a + fromEnum b) `mod` 3)

Refl a |> Refl b = Rotn (a `axisAxis` b)
  where
    axisAxis a b | a == b                                 = Nought
                 | toEnum ((fromEnum a + 1) `mod` 3) == b = TwoForty
                 | otherwise                              = OneTwenty

Rotn a |> Refl b = Refl (a `turnAxis` b)
  where
    turnAxis a b = toEnum ((fromEnum a + fromEnum b) `mod` 3)

Refl a |> Rotn b = Refl (a `axisTurn` b)
  where
    axisTurn a b =  toEnum ((fromEnum a + fromEnum (neg b)) `mod` 3)

neg :: Turn     -> Turn
neg    Nought    = Nought
neg    OneTwenty = TwoForty
neg    TwoForty  = OneTwenty

instance Semigroup EqTriSym where
  (<>) = (|>)

instance Monoid EqTriSym where
  mempty = (Rotn Nought)

instance Group EqTriSym where
  ginverse (Rotn a) = Rotn (neg a)
  ginverse (Refl r) = Refl r

-- Q3

all', any' ::  Foldable t => (a -> Bool) -> t a -> Bool
all' f xs = getAll (mconcat (foldr ((:) . (\x -> All (f x))) [] xs)) 
any' f xs = getAny (mconcat (foldr ((:) . (\x -> Any (f x))) [] xs)) 


all'', any'' :: Foldable t => (a -> Bool) -> t a -> Bool
all'' = compact All
any'' = compact Any

class Extractable a where
  extract :: a -> Bool

instance Extractable All where
  extract = getAll

instance Extractable Any where
  extract = getAny

compact :: (Foldable t, Monoid b, Extractable b) => (Bool -> b) -> (a -> Bool) -> t a -> Bool
compact fromB p xs = extract (mconcat (foldr ((:) . (\x -> fromB (p x))) [] xs))


-- Q4

infixr 0 :=: -- the fixity and priority of the operator
data ProofLayout a = QED | a :=: ProofLayout a deriving Show
instance Foldable ProofLayout where
  foldr f z = ffz
    where
      ffz QED        = z
      ffz (p :=: pl) = f p (ffz pl)
testPL :: Eq a => ProofLayout a -> Bool
testPL QED        = True
testPL (p :=: pl) = all (==p) pl

  

data Nat = Zero | Succ Nat deriving (Eq, Show)
oneN, twoN, threeN :: Nat
oneN   = Succ Zero -- oneN.0
twoN   = Succ oneN -- twoN.0
threeN = Succ twoN -- threeN.0

{-
## Q4.1
-}

(/+/), (/*/) :: Nat -> Nat -> Nat
sqn :: Nat -> Nat

(/+/) x (Succ y) = (Succ x) /+/ y 
(/+/) Zero Zero = Zero
(/+/) x Zero = x

(/*/) x (Succ y) = x /+/ (x /*/ y)
(/*/) _ Zero = Zero


sqn x = x /*/ x


{-
## Q4.2
-}

unitMul :: Nat -> ProofLayout Nat
unitMul Zero =
  oneN /*/ Zero :=:
  Zero :=:
  QED

unitMul (Succ x) =
  oneN /*/ (Succ x) :=:
  (Succ Zero) /*/ (Succ x) :=:
  (Succ x) /*/ (Succ Zero) :=:
  (Succ x) /+/ (Succ x) /*/ Zero :=:
  (Succ x) /+/ Zero :=:
  (Succ x) :=:
  QED

{-
## Q4.3
-}

isOddNat :: Nat -> Bool
isOddNat Zero = False
isOddNat (Succ x) = not (isOddNat x)

sumOdd :: Nat -> Nat
sumOdd Zero = Zero
sumOdd (Succ x) | isOddNat x = x /+/ (sumOdd x)
                | otherwise = sumOdd x
