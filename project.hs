import Data.Time
import qualified  Data.Map as Map
import Data.Maybe (isNothing)

data MembershipType = Standard | Premium deriving (Show, Eq, Ord)

data Member = Member {
    memberId :: Int,
    name :: String,
    membershipType :: MembershipType,
    email :: String
    } deriving (Show)

data Borrow = Borrow {
    bookTitle :: String,
    author :: String,
    loanDate :: Day,
    returnDate :: Maybe Day,
    memberInfo :: Member
    } deriving (Show)

type Members = Map.Map Int Member
type Borrows = [Borrow]

data Library = Library {
    members :: Members,
    loanRecords :: Borrows
} deriving (Show)

registerMember :: Library  -> String -> MembershipType -> String -> Library 
registerMember library memName memType memEmail =
    let memId = Map.size (members library) + 1
        member = Member memId memName memType memEmail
    in library {members = Map.insert memId member (members library)}
    
    
borrowBook :: Library -> String -> String -> Day -> Int -> Either String Library
borrowBook library bookTitle bookAuthor loanDate memId = 
            case Map.lookup memId (members library) of
                Nothing -> Left "Couldn't find the member"
                Just member -> 
                    let memberLoans = filter ((==memId) . memberId . memberInfo) (loanRecords library)
                        maxLoans = case membershipType member of
                            Standard -> 3
                            Premium -> 6
                    in if length memberLoans >= maxLoans 
                        then Left "Borrowing limit exceeded"
                        else let bookToBorrow = Borrow bookTitle bookAuthor loanDate Nothing member
                            in Right library { loanRecords = bookToBorrow : loanRecords library }
             
                            
returnBook :: Library -> String -> Day -> Library
returnBook library title date =
    let updatedLoans = map updateLoan (loanRecords library)
        updateLoan borrow =
            if bookTitle borrow == title && isNothing (returnDate borrow)
                then borrow { returnDate = Just date}
                else borrow
    in library { loanRecords = updatedLoans } 
        

checkLoanStatus :: Library -> String -> String
checkLoanStatus library title = 
    let loan = filter ((==title) . bookTitle) (loanRecords library) 
    in case loan of
        [] -> "Book is available"
        selectedBook -> if isNothing (returnDate (head selectedBook)) then "Book is not available"
             else "Book is available"

applyMembershipDiscount :: Either String Library -> Int -> Double -> String 
applyMembershipDiscount (Right library) mId fee = 
    case Map.lookup mId (members library) of
        Nothing -> "Member not found"
        Just member -> 
            let discountList = Map.fromList [(Standard, 0.0), (Premium, 0.25)]
                discount = case Map.lookup (membershipType member) discountList of
                             Nothing -> 0 
                             Just d -> d 
            in "discounted price: " ++ (show (fee * (1 - discount)))
applyMembershipDiscount (Left a) _ _ = a 



totalActiveMembers :: Library -> Int
totalActiveMembers library = length $ Map.elems $ members library


averageBookBorrowedByMember :: Library -> Double
averageBookBorrowedByMember library =
    let totalBorrows = length (loanRecords library)
        memberCount = Map.size (members library)
    in if memberCount == 0 then 0 else fromIntegral totalBorrows / fromIntegral memberCount


mostPopularBooks :: Library -> [(String, Int)]
mostPopularBooks library =
    let borrowCounts = foldr (\borrow acc -> Map.insertWith (+) (bookTitle borrow) 1 acc) Map.empty (loanRecords library)
    in Map.toList $ Map.filter (> 1) borrowCounts 




main :: IO ()
main = do
    currentTime <- getCurrentTime
    let formattedTime = utctDay currentTime
    
    let initLib = Library Map.empty []
    let mem1 = registerMember initLib "Ariq" Premium "Ariq@gmail.com"
    let mem2 = registerMember mem1 "Kassiyet" Standard "Kassiyet@gmail.com"
    let mem3 = registerMember mem2 "Amir" Premium "Amir@gmail.com"

    let loan1 = borrowBook mem3 "Harry Potter" "J.K. Rolling" (read "2024-11-15") 1
    let loan2 = loan1 >>= (\x -> borrowBook x "Lord Of the Ring" "J.R.R Tolkein" (read "2024-10-21") 1)
    let loan3 = loan2 >>= (\x -> borrowBook x "The Great Gatsby" "F Scott Fitzgerald" (read "2024-11-13") 1)
    let loan4 = loan3 >>= (\x -> borrowBook x "The Catcher In The Rye" "J.D. Salinger" (read "2024-11-10") 2)
    let loan5 = loan4 >>= (\x -> borrowBook x "1984" "George Orwell" (read "2024-9-9") 2)
    let loan6 = loan5 >>= (\x -> borrowBook x "To Kill a Mockingbird" "Harper Lee" (read "2024-11-14") 2)
    let loan7 = loan6 >>= (\x -> borrowBook x "The Great Gatsby" "F Scott Fitzgerald" (read "2024-11-17") 3)


    let return1 = fmap (\x -> returnBook x "The Great Gatsby" (read "2024-11-15")) loan7
    let return2 = fmap (\x -> returnBook x "To Kill a Mockingbird" (read "2024-11-18")) return1



    let check = fmap (\x -> checkLoanStatus x "Harry Potter") return2
    print ( (\x -> "check if Harry Potter is available: " ++ x) <$> check )

    let discount = applyMembershipDiscount return2 1 600
    print $ discount ++ " (original price is 600)"

    let activeMembers = fmap (\x -> totalActiveMembers x) return2
    print ( (\x -> "total active members: " ++ show x) <$> activeMembers)

    let average_book_borrowed_by_member = fmap (\x -> averageBookBorrowedByMember x) return2
    print ( (\x -> "average book borrowed by members: " ++ show x) <$> average_book_borrowed_by_member)
    
    let most_popular_books = fmap (\x -> mostPopularBooks x) return2
    print ( (\x -> "most popular books: " ++ show x) <$> most_popular_books)