-- Haskell Library Membership Management System

module LibrarySystem where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sortOn, group, maximumBy)
import Data.Maybe (isNothing)
import Data.Time (Day)

-- Data Structures

data MembershipType = Standard | Premium deriving (Show, Eq)

data Member = Member
    { memberId :: Int
    , name :: String
    , membershipType :: MembershipType
    , email :: String
    } deriving (Show)


data LoanRecord = LoanRecord
    { bookTitle :: String
    , author :: String
    , loanDate :: Day
    , returnDate :: Maybe Day
    , memberInfo :: Member
    } deriving (Show)

-- Sample Data

type MembersDB = Map Int Member
type LoanDB = [LoanRecord]

data LibrarySystem = LibrarySystem
    { members :: MembersDB
    , loans :: LoanDB
    } deriving (Show)

-- Functions

-- 1. Register Member
registerMember :: LibrarySystem -> String -> MembershipType -> String -> LibrarySystem
registerMember system memberName mType memberEmail =
    let newId = if Map.null (members system) then 1 else fst (Map.findMax (members system)) + 1
        newMember = Member newId memberName mType memberEmail
    in system { members = Map.insert newId newMember (members system) }

-- 2. Borrow Book
borrowBook :: LibrarySystem -> String -> String -> Day -> Int -> Either String LibrarySystem
borrowBook system title author date mId =
    --member validation
  case Map.lookup mId (members system) of
    Nothing -> Left "Invalid Member ID."
    Just member ->
      let memberLoans = filter ((== mId) . memberId . memberInfo) (loans system)
          maxLoans = case membershipType member of
            Standard -> 3
            Premium -> 5
      in if length memberLoans >= maxLoans
         then Left "Borrowing limit exceeded."
         else Right system { loans = LoanRecord title author date Nothing member : loans system }

-- 3. Return Book
returnBook :: LibrarySystem -> String -> Day -> LibrarySystem
returnBook system title returnDate =
  let updatedLoans = map updateLoan (loans system)
      updateLoan loan =
        if bookTitle loan == title && isNothing (returnDate loan)
        then loan { returnDate = Just returnDate }
        else loan
  in system { loans = updatedLoans }


-- 4. Check Loan Status
checkLoanStatus :: LibrarySystem -> String -> String
checkLoanStatus system title =
  case filter ((== title) . bookTitle) (loans system) of
    [] -> "Book is available."
    records -> if any (isNothing . returnDate) records
               then "Book is currently borrowed."
               else "Book is available."

-- 5. Apply Membership Discount
applyDiscount :: LibrarySystem -> Int -> Double -> Either String Double
applyDiscount system mId fee =
  case Map.lookup mId (members system) of
    Nothing -> Left "Invalid Member ID."
    Just member ->
      let discountMap = Map.fromList [(Standard, 0.0), (Premium, 0.2)]
      in Right $ fee * (1 - Map.findWithDefault 0 (membershipType member) discountMap)

-- 6. Generate Membership Statistics
generateStatistics :: LibrarySystem -> (Int, Double, [String])
generateStatistics system =
  let activeMembers = length (Map.elems $ members system)
      totalBooksBorrowed = fromIntegral $ length (loans system)
      avgBooksPerMember = totalBooksBorrowed / fromIntegral activeMembers
      bookFrequency = map head . sortOn (negate . length) . group . map bookTitle $ loans system
   in (activeMembers, avgBooksPerMember, take 3 bookFrequency)

-- Higher-order functions for filtering and mapping data
filterLoansByMember :: Int -> LoanDB -> LoanDB
filterLoansByMember mId = filter ((== mId) . memberId . memberInfo)

-- Error handling and Functor usage
safeMemberLookup :: Int -> MembersDB -> Either String Member
safeMemberLookup mId db = maybe (Left "Member not found.") Right (Map.lookup mId db)

-- Usage example
main :: IO ()
main = do
  let initialSystem = LibrarySystem { members = Map.empty, loans = [] }
  let updatedSystem = registerMember initialSystem "Alice" Premium "alice@example.com"
  print $ members updatedSystem