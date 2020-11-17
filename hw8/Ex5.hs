module Ex5 where

import Employee
import Ex1
import Ex2
import Ex3
import Ex4

getFun :: GuestList -> Fun
getFun (GL _ f) = f

getEmpList :: GuestList -> [Employee]
getEmpList (GL xs _) = xs

main = do
       companyStr <- readFile "company.txt"
       let gl = maxFun . read $ companyStr 
       putStrLn $ "Total fun: " ++ (show . getFun $ gl)
       mapM_ putStrLn . map empName . getEmpList $ gl