module Metagen.MetaParsePatch
    (parseType, parseExp)
where

import qualified Data.List                              as List
import           Language.Haskell.Exts.Extension
import qualified Language.Haskell.Exts.Extension        as Exts
import qualified Language.Haskell.Exts.Parser           as Exts
import qualified Language.Haskell.Exts.SrcLoc           as Exts
import qualified Language.Haskell.Exts.Syntax           as Exts
import qualified Language.Haskell.Meta.Parse            as MetaParse
import qualified Language.Haskell.Meta.Syntax.Translate as MetaParse

toExp' = either Left (Right . MetaParse.toExp)
toType' = either Left (Right . MetaParse.toType)

parseHsExp :: Exts.ParseMode -> String -> Either String (Exts.Exp Exts.SrcSpanInfo)
parseHsExp pm = MetaParse.parseResultToEither . Exts.parseExpWithMode pm

parseHsType ::  Exts.ParseMode -> String -> Either String (Exts.Type Exts.SrcSpanInfo)
parseHsType pm = MetaParse.parseResultToEither . Exts.parseTypeWithMode pm

mkParseMode ::  [KnownExtension] -> Exts.ParseMode
mkParseMode exts = Exts.defaultParseMode
    { Exts.extensions = List.nub $ Exts.extensions Exts.defaultParseMode ++ map EnableExtension exts}

-- better APIs:
-- parseType extensions  = toType' . parseHsType (mkParseMode extensions)
-- parseExp extensions = toExp' . parseHsExp (mkParseMode extensions)

parseType = toType' . parseHsType (mkParseMode litNatExts)
parseExp = toExp' . parseHsExp (mkParseMode litNatExts)

litNatExts :: [KnownExtension]
litNatExts = [ PostfixOperators
             , QuasiQuotes
             , UnicodeSyntax
             , PatternSignatures
             , MagicHash
             , ForeignFunctionInterface
             , TemplateHaskell
             , RankNTypes
             , MultiParamTypeClasses
             , RecursiveDo
             , DataKinds
             ]
