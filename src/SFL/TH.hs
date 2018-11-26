{-# LANGUAGE TemplateHaskell #-}
module SFL.TH where

import           Control.Monad
import           Control.Monad.IO.Class
import           Language.Haskell.TH
import Debug.Trace
import           SFL.Type
import Text.Casing

deriveRecordField :: Name -> DecsQ
deriveRecordField r = do
  DataD _ _ _ _ cs _ <- reify r >>= \case
    TyConI d -> pure d
    _        -> fail $ "Expected data type, but " ++ show r ++ " is not one."
  fields <- case cs of
    [RecC n vbts] -> pure $ (\(v,b,t) -> (v,t)) <$> vbts
    _ -> fail $ "Expected record type, but " ++ show r ++ " is not one."

  let recordFieldTypeName = mkName $ "RecordField" ++ nameBase r
  recordFields <- forM fields $ \(n,t) -> do
    let name = makeRecordFieldConstructorName n
        stringName = show n
    (valueType, isInt) <- case t of
      ConT n' -> do
        (isNum, isInt) <- (,) <$> isInstance ''Num [t] <*> isInstance ''Integral [t] :: Q (Bool, Bool)
        if
          | isNum -> pure ('NumberType, isInt)
          | t == ConT ''String -> pure ('StringType, False)
      _ -> fail $ "Expected simple type for field " ++ show n
    pure (name, stringName, valueType, isInt)
  

  let decInstanceTyped = InstanceD Nothing [] (ConT recordFieldTypeName) 



  pure
    [ mkDecRecordField $ (\(c,_,_,_) -> NormalC c []) <$> recordFields
    , mkInstanceTyped recordFields
    ]
  where
    recordFieldTypeName = mkName $ "RecordField" ++ nameBase r
    mkDecRecordField cons = DataD [] recordFieldTypeName [] Nothing cons [DerivClause Nothing [ConT ''Enum, ConT ''Bounded]]
    mkInstanceTyped xs =
      let clause x t = Clause [ ConP x [] ] (NormalB (ConE t)) []
          instanceD =  InstanceD Nothing [] (AppT (ConT ''Typed) $ ConT recordFieldTypeName)
      in instanceD [FunD (mkName $ nameBase 'typeOf) [clause x t | (x,_,t,_) <- xs]]
    makeRecordFieldConstructorName fieldName = mkName . pascal $ nameBase fieldName
    trace' x = trace (show x) x
