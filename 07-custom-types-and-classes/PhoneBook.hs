module PhoneBook
( PhoneBook(..)
) where

type Name = String
type PhoneNumber = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: (Name, PhoneNumber) -> PhoneBook -> Bool
inPhoneBook (name, phoneNumber) phoneBook = (name, phoneNumber) `elem` phoneBook
