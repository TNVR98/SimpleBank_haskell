module Parser where
import Account

-- converts the string of data from the txt file to Account data type
parse:: String -> [Account]
parse s = map (toAccount. words) $ lines s

-- convert list of string to Account data type
toAccount :: [String] -> Account  
toAccount [u,p,b] = Account u p (read b) 

-- convert the list of account type to string to write it back to the txt file
accountListToStringList :: [Account] -> String 
accountListToStringList accs = unlines $map toString accs

toString :: Account -> String
toString (Account u p b) = unwords [u , p,show b] 