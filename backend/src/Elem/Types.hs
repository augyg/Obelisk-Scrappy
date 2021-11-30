{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
-- <-- Only for RecursiveTree a b c ; remove if changed 
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Elem.Types where

import Data.Text (Text, unpack)
import Data.Map (Map, toList)
import qualified Data.Map as Map

import Data.Graph (Tree (Node), Forest)
import Text.Megaparsec as MParsec (some)
import Text.Parsec (ParsecT, Stream, parserZero, string, (<|>), anyChar, char, optional, try, manyTill, alphaNum
                   , parserFail)
import Data.Maybe (fromMaybe)
-- Goals: eliminate need for sub tree and inner datatypes ; so Element --> foldr [HTMLMatcher] empty :: Element 


----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- MAYBE:
class HtmlMatcher a where
  --defines relation between Element -> ITR 

-- practically just show but we dont wanna restrict the entirety
-- of the show class for any sub-datatype in the context of html
-- laws:
  -- in the expression:
       -- x <- parser input
  -- showH x == input
-- This is useful for extracting values while retaining structure for successive parsing 
class ShowHTML a where
  showH :: a -> String

instance ShowHTML String where
  showH x = x

instance ShowHTML Text where
  showH = unpack 

-- a b c are the three main types
-- class RecursiveTree a b c where
--   type (Element c)
--   type (InnerForest c)--   type (HMatcher c) -- may not need this tho
                 
--   toHtmlGen :: Element c -> HMatcher c
--   elToITR :: Element c -> InnerForest c
--   htmlGenToITR :: HMatcher c -> InnerForest c 
--   concatForest :: InnerForest c -> InnerForest c -> InnerForest c
--    -- must be instance of monoid -- dont need to declare each time 
--   label :: ElemHead -> InnerForest c -> Element c

-- need system too for 
-- Just shared accessors of html datatypes 
class ElementRep (a :: * -> *) where
--type InnerHTMLRep a = something 
  elTag :: a b -> Elem
  attrs :: a b -> Attrs
  innerText' :: a b -> String 
  matches' :: a b -> [b]
--

-- Elem should determine InnerText
-- Summarizes finds as a result of parsing then folding 
-- Recursive fold of multi-constructor structure 
class (ShowHTML c, ElementRep a) => InnerHTMLRep (a :: * -> *)  (b :: * -> *) c | a c -> b c where
  -- type HMatcher a b c :: * -> *
  foldHtmlMatcher :: [HTMLMatcher a c] -> b c
  -- emptyInner :: a d
  innerText :: b c -> String 
  matches :: b c -> [c]


-- f :: ([a], [b], [c]) -> Elem' a
-- f (x,y,z) = f' x y z

-- f' el attrs = Elem' el attrs

-------------------------------------------------------------------------------------------------------------------

instance Semigroup (InnerTextHTMLTree a) where
  InnerTextHTMLTree a b c <> InnerTextHTMLTree d e f = InnerTextHTMLTree (a <> d) (b <> e) (c <> f)

instance Monoid (InnerTextHTMLTree a) where
  mempty = InnerTextHTMLTree { _matches = [], _innerText = "", innerTree = [] }

instance Semigroup (InnerTextResult a) where
  InnerTextResult a b <> InnerTextResult a' b' = InnerTextResult (a <> a') (b <> b')

instance Monoid (InnerTextResult a) where
  mempty = InnerTextResult { _matchesITR = [], _fullInner = "" }

-- instance Num GroupHtml e a where
--   GroupHtml


--Note that the fullInner would be useful for stepping in cases as well needing some pieces of the
-- whole, in the case, that theres a match (like a form: want a form if it has search in it, then
-- we'd want to use certain html elements within the form such as <input> or <select>
--so result would be : InnerTextResult { match = "Search", fullInner = """<input> ... <select> """ } 

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- foldFuncToITR' :: [Elem' a] -> InnerTextResult a -> InnerTextResult a
-- foldFuncToITR' InnerTextResult{..} (Elem'{..}:xs) =
--   func (InnerTextResult (matchesITR <> matchesEl) (fullInner <> innerHtmlFull)) xs 

-- case x of
  -- Elem' x -> [Element (Elem' x)]



-- foldElements' :: [Elem' a] -> InnerTextResult a
-- foldElements' elems = foldr func (mempty :: InnerTextResult a) (fmap Element elems) 




-- | Tree
-- func :: Show a => [HTMLMatcher a] -> InnerTextHTMLTree a -> InnerTextHTMLTree a
--     func [] state = state
--     func (htmlM:htmlMatchers) (InnerTextHTMLTree{..}) = case htmlM of
--       IText str -> 
--         func htmlMatchers (InnerTextHTMLTree matches (innerText <> str) innerTree)
--       -- | May need to enforce a Show Instance on 'mat'
--       Match mat -> 
--         func htmlMatchers (InnerTextHTMLTree (mat : matches) (innerText <> (show mat)) innerTree)
--       --concat to fullInnerText
--       Element htmlTree -> --interEl :: ElemHead [HtmlMatcher]
--         func htmlMatchers (InnerTextHTMLTree (matches <> matches' htmlTree) (innerText <> treeElemToStr htmlTree) ((makeBranch htmlTree) : innerTree))


-- | SimpleElem
--     func :: (ShowHTML a, ElementRep e) => [HTMLMatcher e a] -> InnerTextResult a
--     func state [] = state
--     func InnerTextResult{..} (next:inners) = case next of
--       IText str -> 
--         func (InnerTextResult matches (innerText <> str)) inners 
--       -- | May need to enforce a Show Instance on 'mat'
--       Match mat -> 
--         func (InnerTextResult (mat : matches) (innerText <> (showH mat))) inners 
--       --concat to fullInnerText
--       Element elem  -> --interEl :: ElemHead [HtmlMatcher]
--         func (InnerTextResult (matches' elem <> matches) (innerText <> (innerText' elem))) inners



-- instance InnerHTMLRep Elem' InnerTextHTMLTree c --> deletion of tree; we no longer care for this info
  
instance ShowHTML c => InnerHTMLRep TreeHTML InnerTextHTMLTree c where
  -- type HMatcher TreeHTML InnerTextHTMLTree c = HMatcher' c 
  -- emptyInner = InnerTextHTMLTree { matches = []
  --                                , innerText = ""
  --                                , innerTree = [] }
  foldHtmlMatcher = foldr fHM_c mempty -- original one
  matches = _matches
  innerText = _innerText 
  
instance ShowHTML c => InnerHTMLRep Elem' InnerTextResult c where
  foldHtmlMatcher = foldr foldFuncITR mempty -- partially applied, expecting some a 
  matches = _matchesITR
  innerText = _fullInner 

-- instance InnerHTMLRep InnerTextResult where
--   type HtmlMatcherM a = HTMLMatcher Elem' a
--   foldHtmlMatcher = foldInners



instance ElementRep (Elem') where
  elTag = _el
  attrs = _attrs
  innerText' = innerHtmlFull
  matches' = innerMatches

instance ElementRep (TreeHTML) where
  elTag = _topEl
  attrs = _topAttrs
  innerText' = _innerText'
  matches' = _matches'





instance ShowHTML a => ShowHTML (Elem' a) where
  showH = elemToStr 



instance ShowHTML a => ShowHTML (TreeHTML a) where
  showH = treeElemToStr

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
   



    
-------------------------------------------------------------------------------------------------------------------


-- | Note, this is the representation i'll be using
data TreeHTML a = TreeHTML { _topEl :: Elem
                           , _topAttrs :: Map String String
                           --
                           , _matches' :: [a]
                           , _innerText' :: String
                           , _innerTree' :: Forest ElemHead 
                           } deriving Show 


---Future: Make both below into semigroups
data InnerTextHTMLTree a = InnerTextHTMLTree { _matches :: [a]
                                             , _innerText :: String
                                             , innerTree :: Forest ElemHead
                                             } 
-------------------------------------------------------------------------------------------------------------------
-- | node-like
data Elem' a = Elem' { _el :: Elem -- change to Elem?
                     , _attrs :: Map String String --Attrs needs to become map
                     , innerMatches :: [a] --will be "" if not specified
                     , innerHtmlFull :: String
                     } deriving Show

data InnerTextResult a = InnerTextResult { _matchesITR :: [a]
                                         , _fullInner :: String -- set to Nothing if TextOnly
                                         } deriving Show
-------------------------------------------------------------------------------------------------------------------

type HMatcher' a b c = [HTMLMatcher b c] -> a c





data HTMLMatcher (a :: * -> *) b = IText String | Element (a b) | Match b deriving Show







type HTMLMatcherM a = HTMLMatcher TreeHTML a
-- data HTMLMatcher a = IText String | Match a | Element (TreeHTML a)
type Inner a = HTMLMatcher Elem' a


type HTMLMatcherList a = HTMLMatcher [] a
-- IText String | Match a | Element [] a 



-- mainly for testing
-- allows for minimal steps and ensuring that low level parsing is working
data HTMLBare e a = HTMLBare { tag :: Elem
                             , attrsss :: Attrs
                             , htmlM :: [HTMLMatcher e a]
                             }



type ElemHead = (Elem, Attrs) 
type Attrs = Map String String
type Elem = String


data AttrsError = IncorrectAttrs deriving Show


instance Eq (GroupHtml e a) where
  GroupHtml _ gl1 ml1 == GroupHtml _ gl2 ml2 = (gl1 * ml1) == (gl2 * ml2)

-- allows for max
instance Ord (GroupHtml e a) where
  --  compare multiples of Glength * MaxLength
  (GroupHtml _ gl1 ml1) <= (GroupHtml _ gl2 ml2) = (gl1 * ml1)  <= (gl2 * ml2)

--Note, maybe there's a way to extend this to being capable of gathering all text-distinct pieces of
--the page

-- | At end will be able to do Eq2 of trees where tree params are (tag,attrs)
-- | Need some "Flexible Equality" match







-- | Would treeElemParser fail on cases like <input> with no end tag? 



-------------------------------------------------------------------------------------------------------------------
-------------------------------------Experimental---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

data MessyTree a b = Noise b | Nodee a [MessyTree a b]
-- where the data at each node, describes the branch .. eg ElemHead
data MessyTreeMatch a b c = Noise' a | Match' b | Node' c [MessyTreeMatch a b c] 


type TreeIndex = [Int]
-- What if we made trees a lense? accessible through treeindex 

-------------------------------------------------------------------------------------------------------------------
-------------------------------------Experimental---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- data GroupHtml a = GroupHtml [a] Glength MaxLength deriving Eq -- ... | Nil
data GroupHtml element a = GroupHtml [element a] Glength MaxLength

instance (ElementRep e, Show (e a), Show a, ShowHTML a) => Show (GroupHtml (e :: * -> *) a) where
  show (GroupHtml (e:_) count maxELen) =
    "GroupHtml { count =" <> (show count) <> ", longestElem= " <> show maxELen <> ", elemStructure=" <> (show e)

type Glength = Int -- number of items in group
type MaxLength = Int -- could also just be length of head 



-- a ~ TreeHTML match
ungroup :: ElementRep e => GroupHtml e a -> [e a]
ungroup (GroupHtml xs _ _) = xs 



mkGH :: ElementRep e => [e a] -> GroupHtml e a
mkGH result = GroupHtml result (length result) ((length (innerText' (head result))))


-- so then id go from

  -- Just GroupHtml { count =20, longestElem= 6047, elemStructure=TreeHTML .. 

-- to:

  -- (e,a) ; in this case -> ("li", [("class", "resultItem ltr")])

              ---- > put to state
              ---- > In order to be safe: findNaive (elemParser "li"..) 
              ---- > with innerText's ; do findNaive linkScraper
                 ---- > 
  

longestElem :: [Elem' a] -> Maybe (Elem' a)
longestElem [] = Nothing
longestElem (a:[]) = Just a
longestElem (x:xs) = if length (innerText' x) > length (innerText' $ head xs)
                     then longestElem (x: (tail xs))
                     else longestElem xs



maxLength :: [[a]] -> [a]
maxLength (a:[]) = a
maxLength (x:xs) = if length x > length (head xs) then maxLength (x: (tail xs)) else maxLength xs



biggestHtmlGroup :: [GroupHtml e a] -> GroupHtml e a 
biggestHtmlGroup ghs = foldr maxE (GroupHtml [] 0 0) ghs
  where
    maxE :: GroupHtml e a -> GroupHtml e a -> GroupHtml e a -- like max elem
    maxE (GroupHtml xs cnt lng) (GroupHtml ys cnt' lng') =
      if (cnt * lng) > (cnt' * lng')
      then (GroupHtml xs cnt lng)
      else (GroupHtml ys cnt' lng')
           

-- Note: findAllMutExGroups 
biggestGroup :: ElementRep e => [GroupHtml e a] -> GroupHtml e a
biggestGroup (gh:[]) = gh 
biggestGroup (n0@(GroupHtml a x1 y1) :n1@(GroupHtml b x2 y2):ghs) = case (x1 * y1) > (x2 * y2) of
  True -> biggestGroup (n0:ghs)
  False -> biggestGroup (n1:ghs)
  


getHref :: ElementRep e => e a -> Maybe String
getHref e = ((Map.lookup "href") . attrs) e



-- f
elemToStr :: Elem' a -> String
elemToStr elem = "<" <> elTag elem <> buildAttrs (toList (attrs elem)) <> ">" <> innerHtmlFull elem <> "</" <> elTag elem <> ">"
  where
    buildAttrs [] = ""
    buildAttrs (attr:attrss) = " " <> fst attr <> "=" <> "\"" <> snd attr <> "\"" <> buildAttrs attrss


treeElemToStr :: (ShowHTML a) => TreeHTML a -> String
treeElemToStr (TreeHTML{..}) =
  "<" <> _topEl <> mdToStringPairs _topAttrs <> ">" <> _innerText'  <> "</" <>  _topEl <> ">"
  where mdToStringPairs attrsSet = go (toList attrsSet)
        go [] = ""
        go (atr:[])        = (fst atr) <> "=" <> ('"' : snd atr) <> ('"' : []) 
        go (atr: attrsSet) = (fst atr) <> "=" <> ('"' : snd atr) <> ('"' : []) <> go attrsSet


-- Future concern:
  -- elem with tree as [Elem a] --> due to lazy evaluation, we dont actually need to summarize these, can just
  -- leave as is, then the user can execute a command
        --- especially easily with accompanying
        -- type TreeIndex = [Int] ; eg [1,4,7] 1st el then 4th el then 7th el

-- Future concern: just discards non matching tokensx
foldFuncMatchlist :: (ShowHTML a, ElementRep e) => HTMLMatcher e a -> InnerTextResult a -> InnerTextResult a
foldFuncMatchlist hMatcher itr = undefined --case hMatcher of
--   IText str -> 
--     InnerTextResult (_matchesITR itr) (_fullInner itr <> str)
--   -- | May need to enforce a Show Instance on 'mat'
--   Match mat -> 
--     InnerTextResult (mat : _matchesITR itr) (_fullInner itr <> (showH mat))
--   --concat to fullInnerText
--   Element elem  -> --interEl :: ElemHead [HtmlMatcher]
--     InnerTextResult (matches' elem <> _matchesITR itr) (_fullInner itr <> (innerText' elem))

--------------------------------------------------------------------------------------------------------------------Testing-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- | Bug found: matches start right way then get reversed 

foldFuncTup :: (ShowHTML (e a), ShowHTML a, ElementRep e) => HTMLMatcher e a -> (String, [a]) -> (String, [a]) 
foldFuncTup hMatcher itr = case hMatcher of
   IText str -> 
    (fst itr <> str, snd itr)
    --  May need to enforce a Show Instance on 'mat'
   Match mat -> 
    (fst itr <> (reverse $ showH mat), snd itr <> [mat])
  --concat to fullInnerText
   Element elem  -> --interEl :: ElemHead [HtmlMatcher]
    (fst itr <> (reverse $ showH elem), snd itr <> matches' elem)

foldFuncTrup :: (ShowHTML a) => HTMLMatcher TreeHTML a -> (String, [a], Forest ElemHead) -> (String, [a], Forest ElemHead) 
foldFuncTrup hMatcher itr = case hMatcher of
  IText str -> 
    (fst' itr <> str, snd' itr, thd' itr)
    --  May need to enforce a Show Instance on 'mat'
  Match mat -> 
    (fst' itr <> (showH mat), snd' itr <> [mat], thd' itr)
  --concat to fullInnerText
  Element elem  -> --interEl :: ElemHead [HtmlMatcher]
    (fst' itr <> (reverse $ showH elem), snd' itr <> matches' elem, thd' itr <> ((makeBranch elem):[]))

-- | In our failed test case with the command : parse f "" "<a></div></a>"
  -- where f :: (Stream s m Char) => ParsecT s u m (TreeHTML String); f = treeElemParser (Just ["a"]) Nothing []
  --
  -- we can tell that foldFuncTrup has been called twice (we believe)
  --
  -- we will test how an element named "div" inside of "a" element would behave


 

-- -- |data ElemHead = (Elem/Text, Attrs)
-- -- | Note: Attrs will be changed to being a Map String String
-- makeBranch :: ShowHTML a => TreeHTML a -> Tree ElemHead
-- makeBranch treeH = Node (elTag treeH, attrs treeH) (_innerTree' treeH)


----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

fst' :: (a, b, c) -> a
fst' (a,_,_) = a

snd' :: (a,b,c) -> b
snd' (_,b,_) = b

thd' :: (a,b,c) -> c
thd' (_,_,c) = c

foldFuncITR :: (ShowHTML a, ElementRep e) => HTMLMatcher e a -> InnerTextResult a -> InnerTextResult a
foldFuncITR hMatcher itr = case hMatcher of
  IText str -> 
    InnerTextResult (_matchesITR itr) (_fullInner itr <> str)
    --  May need to enforce a Show Instance on 'mat'
  Match mat -> 
    InnerTextResult (mat : _matchesITR itr) (_fullInner itr <> (showH mat))
  --concat to fullInnerText
  Element elem  -> --interEl :: ElemHead [HtmlMatcher]
    InnerTextResult (matches' elem <> _matchesITR itr) (_fullInner itr <> (innerText' elem))


-- foldFuncITHT :: (ShowHTML a, ElementRep e) => HTMLMatcher TreeHTML a -> InnerTextResult a -> InnerTextResult a
-- We could also implement (with a proper class system, a way to fold to InnerTextResult not just tree
  -- has performance benefits
  
fHM_c :: (InnerHTMLRep TreeHTML InnerTextHTMLTree a, ShowHTML a) =>
         HTMLMatcher TreeHTML a
      -> InnerTextHTMLTree a
      -> InnerTextHTMLTree a
fHM_c hMatcher ithT = case hMatcher of 
  IText str -> 
    InnerTextHTMLTree (_matches ithT) (_innerText ithT <> str) (innerTree ithT)
      --  May need to enforce a Show Instance on 'mat'
  Match mat -> 
    InnerTextHTMLTree (mat : (_matches ithT)) (_innerText ithT <> (showH mat)) (innerTree ithT)
      --concat to fullInnerText
  Element htmlTree -> --interEl :: ElemHead [HtmlMatcher]
    InnerTextHTMLTree (_matches ithT <> matches' htmlTree) ((_innerText ithT) <> showH htmlTree) ((makeBranch htmlTree) : innerTree ithT)


makeBranch :: TreeHTML a -> Tree ElemHead
makeBranch treeH = Node (elTag treeH, attrs treeH) (_innerTree' treeH) -- 2 cases: 
                   



endTag :: Stream s m Char => String -> ParsecT s u m String 
endTag elem = try (string ("</" <> elem <> ">"))


enoughMatches :: Int -> String -> Map String String -> (String, [a]) -> ParsecT s u m (Elem' a)
enoughMatches required e a (asString, matches) = 
  if required <= (length matches)
  then return $ Elem' e a matches (reverse asString)
  else parserFail "not enough matches" -- should throw real error 

enoughMatchesTree :: Int -> String -> Map String String -> (String, [a], Forest ElemHead) -> ParsecT s u m (TreeHTML a)
enoughMatchesTree required e a (asString, matches, forest) = 
  if required <= (length matches)
  then return $ TreeHTML e a matches (reverse asString) forest
  else parserFail "not enough matches" -- should throw real error 


-- | This is valid according to my knowledge but need to see if better to
-- maybe instead: do some text >>= (\x -> char '\n' >> x) with source being RequestBody
-- If i recall correctly, self-closing tags dont allow embedded elements
selfClosingTextful :: (ShowHTML a, Stream s m Char) =>
                      Maybe (ParsecT s u m a)
                   -> ParsecT s u m [HTMLMatcher e a]
selfClosingTextful innerP =
  -- Fix: all is prefixed with 
  
  char '>'
  >> manyTill ((try (Match <$> innerP')) <|> (try ((IText . (:[])) <$> anyChar))) ((try anyEndTag) <|> (char '<' >> some alphaNum))
  where anyEndTag = (try (char '<'
                       >> (optional (char '/'))
                       >> MParsec.some anyChar
                       >> (string " " <|> string ">")))
        innerP' = fromMaybe parserZero innerP

-- is (selfClosingTextful Nothing) if no match desired 




---- # DEPRECATED UrlPagination "use CurrentQuery" #
data UrlPagination = UrlPagination String String deriving (Eq, Show)



--   where
--     func :: Show a => [HTMLMatcher a] -> InnerTextHTMLTree a -> InnerTextHTMLTree a
--     func [] state = state
--     func (htmlM:htmlMatchers) (InnerTextHTMLTree{..}) = case htmlM of
--       IText str -> 
--         func htmlMatchers (InnerTextHTMLTree matches (innerText <> str) innerTree)
--       -- | May need to enforce a Show Instance on 'mat'
--       Match mat -> 
--         func htmlMatchers (InnerTextHTMLTree (mat : matches) (innerText <> (show mat)) innerTree)
--       --concat to fullInnerText
--       Element htmlTree -> --interEl :: ElemHead [HtmlMatcher]
--         func htmlMatchers (InnerTextHTMLTree (matches <> matches' htmlTree) (innerText <> treeElemToStr htmlTree) ((makeBranch htmlTree) : innerTree))



  
-- -- In future, I may generalize this along with HtmlMatcher to be something * -> * -> *

-- instance ToHTMLString a => ToHTMLString (Inner a) where
--   showH NonMatch str = str
--   showH IsMatch x = showH x

-- instance ToHTMLString a => ToHTMLString (HTMLMatcher a) where
--   showH IText str = str
--   showH Match x = showH x
--   showH Element treeH = treeElemToStr

-- Behaves just like two are right beside each other 


  

-- Note: I could fold Elem' by labeling as Element then folding to ITR

-- (Element $ elem) : [] --> ITR





-- foldr :: (Elem' -> ITR -> ITR) -> ITR -> [] Element -> ITR 

-- for InnerTextResult, should preserve as string
-- for InnerTextHTMLTree , elem and attrs should build new single branch where branch = (,) elem attrs

-- my recursive functions can build off of this mutual foldability and work for anything that can provide
-- mutual foldability for any two relations 

-- foldFuncToITR :: [Elem' a] -> InnerTextResult a -> InnerTextResult a
-- foldFuncToITR InnerTextResult{..} (Elem'{..}:xs) =
--   func (InnerTextResult (matchesITR <> matchesEl) (fullInner <> innerHtmlFull)) xs 

-- case x of
  -- Elem' x -> [Element (Elem' x)]



-- foldElements :: [Elem' a] -> InnerTextResult a
-- foldElements elems = foldr func (mempty :: InnerTextResult a) (fmap Element elems) 

-- foldr func mempty htmlMatchers 

-- foldrForHTMLGen :: func@(HMatcher a -> ITR -> ITR) -> mempty -> 




-- this builds off the type classification of IsHTMLRep



-- -- Note: Inner a = HtmlMatcher (String, a) a
-- foldInners :: [Inner a] ->  InnerTextResult a
-- foldInners htmlMatchers = func emptyInner htmlMatchers
--   where
--     func :: (ShowHTML a, ElementRep e) => [HTMLMatcher e a] -> InnerTextResult a -> InnerTextResult a
--     func state [] = state
--     func itr (next:inners) = case next of
--       IText str -> 
--         func (InnerTextResult matches (innerText <> str)) inners 
--       -- | May need to enforce a Show Instance on 'mat'
--       Match mat -> 
--         func (InnerTextResult (mat : matches) (innerText <> (showH mat))) inners 
--       --concat to fullInnerText
--       Element elem  -> --interEl :: ElemHead [HtmlMatcher]
--         func (InnerTextResult (matches' elem <> matches itr) (innerText itr <> (innerText' elem))) inners





















-- instance Eq ((->) a b) where
  -- eq fx fx' = f (x:xs) = fx x == fx' x && f xs


--- > A prerequisite would need to be Universe instance 
type Tag = String 
