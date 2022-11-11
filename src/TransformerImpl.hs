-- Put yor transformer implementation in this file
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module TransformerImpl where

import Definitions

convert :: EGrammar -> EM Grammar
convert = undefined

-- convertOneRule :: ERule -> EM Grammar
-- convertOneRule (rlhs, erhs) = Right [(convertRLHS rlhs, convertERHS erhs)]


-- convertRLHS :: RLHS -> RLHS
-- convertRLHS rlhs = rlhs

-- erhs2simple :: ERHS -> Simple
-- erhs2simple (ESimple simple) = simple

-- erhslist2simple :: [ERHS] -> [Simple]
-- erhslist2simple erhslist = case erhslist of
--     x:xs -> erhs2simple x : erhslist2simple xs
--     [] -> []

-- convertERHS :: (RLHS, ERHS) -> [RLHS, [([Simple], Action)]]
-- convertERHS (ESimple simple) = [([simple], AVar "_1")]
-- convertERHS (ESeq erhsList htext) =
--     let simplelist = erhslist2simple erhsList;
--         action = AUser htext
--     in [(simplelist, action)]

-- convertERHS (ENot erhs) = case convertERHS erhs of 
--   [] -> []
--   x : xs -> 



lre :: Grammar -> EM Grammar
lre = undefined

lfactor :: Grammar -> EM Grammar
lfactor = undefined
