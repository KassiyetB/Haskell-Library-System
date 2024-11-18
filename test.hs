module LibrarySystem where

import Data.Map (Map)
--using qualified to use Map.insert instead of insert, to avoid name clashes
import qualified Data.Map as Map
import Data.List (sortOn, group, maximumBy)
import Data.Maybe (isNothing)
import Data.Time (Day)
import Data.Time.Calendar (addDays, fromGregorian)


-- Data Structures

data MembershipType = Standard | Premium deriving (Show)

data Member = Member
    {   memberId :: Int,
        name :: String,
        membershipType :: MembershipType,
        email :: String 
    } deriving (Show)

data LoanRecord = LoanRecord 
    {   bookTitle :: String,
        author :: String,
        loanDate :: Day,
        returnDate :: Maybe Day,
        memberInfo :: Member
    } deriving (Show)



--storing all the members in a map
type MembersDB = Map Int Member
--storing all the loans in a list
type LoanDB = [LoanRecord]


data LibrarySystem = LibrarySystem
    {   members :: MembersDB,
        loans :: LoanDB
    } deriving (Show)

--Register Member
registerMember :: LibrarySystem -> String -> MembershipType -> String -> LibrarySystem
registerMember system memberName mType memberEmail =
    let newId = if Map.null (members system) then 1 else fst (Map.findMax (members system)) + 1
        newMember = Member newId memberName mType memberEmail
    in system {members = Map.insert newId newMember (members system)}

--Borrow Book
-- borrowBook :: LibrarySystem -> String -> String -> Day -> Int -> Either String LibrarySystem
-- borrowBook system title author date mId =
--     --check if the given Id is valid
--     case Map.lookup mId (members system) of
--         Nothing -> Left "Invalid Mebmer ID."
--         Just member ->
--             -- get the Loan records of the specified member
--             let memberLoans = filter ((== mId) . memberId . memberInfo) (loans system)
--                 --assign borrow limit depending on the member's membership type
--                 maxLoans = case membershipType member of 
--                     Standard -> 3
--                     Premium -> 5
--             in if length memberLoans >= maxLoans
--                 then Left "Borrowing limit exceeded."
--                 else Right system { loans = LoanRecord title author date Nothing member : loans system}

borrowBook :: LibrarySystem -> String -> String -> Day -> Int -> LibrarySystem
borrowBook system title author date mId =
    --check if the given Id is valid
    case Map.lookup mId (members system) of
        Just member ->
            -- get the Loan records of the specified member
            let memberLoans = filter ((== mId) . memberId . memberInfo) (loans system)
                --assign borrow limit depending on the member's membership type
                maxLoans = case membershipType member of 
                    Standard -> 3
                    Premium -> 5
            in system { loans = LoanRecord title author date Nothing member : loans system}



--Return Book
returnBook :: LibrarySystem -> String -> Day -> LibrarySystem
returnBook system title date =
    let updatedLoans = map updateLoan (loans system)
        updateLoan loan =
            if bookTitle loan == title && isNothing (returnDate loan)
            then loan {returnDate = Just date }
            else loan
    in system {loans = updatedLoans}










main :: IO()
main = do
    let date = fromGregorian 2024 11 16
    let finishDate = fromGregorian 2024 12 10

    let library = LibrarySystem Map.empty []
    let member1 = registerMember library "Kassi" Premium "hasiet.bolat@gmail.com"
    print member1
    let borrow = borrowBook member1 "Harry Potter" "J.K.Rolling" date 1
    let borrow2 = borrowBook borrow "To kill a mockingbird" "Harper Lee" date 1
    let return = returnBook borrow2 "Harry Potter" finishDate
    print return