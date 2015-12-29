
import qualified Data.MemoCombinators as Memo

m :: Integer
m = 987878789

-- | Calculates the number of tilings of length `n`
t :: Integer -> Integer
t = Memo.integral t'
    where
        -- | Memoizable `t`
        t' :: Integer -> Integer
        t' 0 = 1
        t' 1 = 10
        t' n
            = (t $ n - 1) * 10 -- # ways to end in a unit block
            + (t $ n - 2)      -- # ways to end in a double block

-- | Calculates gcd (t a) (t b)
tgcd :: Integer -> Integer -> Integer
tgcd a b
    | not (odd a && odd b) = 1 -- verified empirically
    | otherwise = gcd (t a) (t b) `mod` m-- TODO
    -- > tgcd (5^2) (5^3)
    -- 10
    -- > tgcd (5^2) (5^4)
    -- 10
    -- > tgcd (5^2) (5^5)
    -- 10
    -- > tgcd (5^2) (5^6)
    -- 12669014278526824700664130
    --
    -- > tgcd (9^2) (9^3)
    -- 10
    -- > tgcd (9^2) (9^4)
    -- 10
    -- > tgcd (9^3) (9^4)
    -- 10
    -- > tgcd (9^3) (9^3)
    -- 130409016740549253100953882270098576189148242702173683030633197415616500852586200937598204320680778051056152045482...