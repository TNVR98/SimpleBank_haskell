module Account where 

--Account data type
data Account = Account Username Password Balance
    deriving (Show, Eq)

type Username= String 
type Password = String
type Balance = Float


--  these 3 funtions are used to allow the user to login given the username and password
searchAccount :: Username -> Password -> [Account] -> Bool
searchAccount usr pass [] = False
searchAccount usr pass (x:xs)
    | checkAccount usr pass x == True = True
    | otherwise = searchAccount usr pass xs

checkAccount :: Username -> Password -> Account -> Bool
checkAccount usr pass (Account u p b) = (usr == u) && (pass == p)



getAccount:: Username -> Password -> [Account] -> Account
getAccount usr pass [x] = x 
getAccount usr pass (x:xs)
    | checkAccount usr pass x == True = x 
    | otherwise = getAccount usr pass xs


-- returns the balance of the user
checkBalance :: Account -> Float
checkBalance (Account u p b) = b 


-- depositMoney :: Float -> Account -> Account
-- depositMoney amount (Account u p b ) = Account u p (b + abs amount)

-- withdrawMoney :: Float -> Account -> Account
-- withdrawMoney amount (Account u p b ) = Account u p (b - abs amount)



-- this generalized function is used when user wants to deposit or withdraw money 
updateBalance :: Float -> (Balance -> Balance-> Balance) -> Account -> Account
updateBalance amount f (Account u p b) = Account u p (f b $abs amount)





-- deleteAccount :: Account -> [Account] -> [Account]
-- deleteAccount _ [] = []
-- deleteAccount (Account u p b ) (x: xs) 
--     | 


-- a concised funtion to delete an account from the account list
deleteAccount :: Account-> [Account] -> [Account]
deleteAccount acc accs = filter (not . isSameAccount acc) accs


-- checks if 2 account types are equivalent
isSameAccount :: Account -> Account -> Bool
isSameAccount x y = x == y 




 -- these 3 functions are used to transfer money from one account to another in Mains.hs 
--  by seaching the recipient account with a username
searchByUsername:: Username -> [Account] -> Bool
searchByUsername usr []= False 
searchByUsername usr (x:xs) 
    | checkByUsername usr x = True
    | otherwise = searchByUsername usr xs


checkByUsername :: Username -> Account -> Bool
checkByUsername usr (Account u p b) = (usr == u)


getByUsername :: Username -> [Account] -> Account
getByUsername usr [x]=x
getByUsername usr (x:xs)
    | checkByUsername usr x = x
    | otherwise = getByUsername usr xs













-- test 

x :: [Account]
x = [Account "Tnr" "pss" 0,Account "Tnvrrr" "pass" 0,Account "Tnvr" "pass" 100,Account "Tnveer" "pass" 0]


y:: Account
y = Account "Tnvr" "pass" 100

z:: Account
z = Account "sim" "pass" 100

-- test 