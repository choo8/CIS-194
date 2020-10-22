import Ex1
import Ex2
import Ex3

validate :: Integer -> Bool
validate xs = if remainder == 0 then True else False
    where remainder = (Ex3.sumDigits . Ex2.doubleEveryOther . Ex1.toDigits $ xs) `mod` 10