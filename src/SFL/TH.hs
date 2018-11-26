{-# LANGUAGE TemplateHaskell #-}
module SFL.TH 
  ( deriveRecordField
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Language.Haskell.TH as TH
import Debug.Trace
import           SFL.Type
import Text.Casing

data FieldInfo = FieldInfo
  { fiName :: Name
  , fiConstructorName :: Name
  , fiStringName :: String
  , fiValueType :: Name
  , fiIsInt :: Bool
  }

baseName :: Name -> Name
baseName = mkName . nameBase

deriveInstanceTyped :: Name -> [FieldInfo] -> Dec
deriveInstanceTyped rf fis = 
  let mkClause FieldInfo{..} = Clause [ ConP fiConstructorName [] ] (NormalB (ConE fiValueType)) []
      instanceD = InstanceD Nothing [] (ConT ''Typed `AppT` ConT rf)
  in instanceD [FunD (baseName 'typeOf) $ mkClause <$> fis]

deriveDataRecordField :: Name -> [FieldInfo] -> Dec
deriveDataRecordField rf fis = 
  let mkCons FieldInfo{..} = NormalC fiConstructorName []
      deriveClauses = 
        [ DerivClause Nothing [ConT ''Enum, ConT ''Bounded] ]
  in DataD [] rf [] Nothing (mkCons <$> fis) deriveClauses

deriveInstanceRecordField :: Name -> Name -> [FieldInfo] -> Dec
deriveInstanceRecordField r rf fis = 
  InstanceD Nothing [] (AppT (ConT ''RecordField) $ ConT rf) [ mkRecordOf, mkFromRecordId, mkToRecordId, mkRecordValue ]
  where
    mkFromRecordId =
      let sn' = LitP . TH.StringL
          mkClause FieldInfo{..} = Clause [ sn' fiStringName ] (NormalB $ AppE (ConE 'Just) (ConE fiConstructorName)) []
      in FunD 'fromRecordId $ 
        (mkClause <$> fis) ++ [ Clause [ WildP ] (NormalB (ConE 'Nothing)) [] ]
    mkToRecordId = FunD 'toRecordId [ Clause [ ConP fiConstructorName [] ] (NormalB . LitE $ TH.StringL fiStringName) [] | FieldInfo{..} <- fis ]
    mkRecordOf = TySynInstD ''RecordOf $ TySynEqn [ConT rf] (ConT r)
    mkRecordValue = 
      let r' = mkName "r"
          mkBody FieldInfo{..}
            | fiValueType == 'StringType = foldr AppE (VarE r') [ ConE 'StringV, VarE fiName ]
            | fiValueType == 'NumberType = foldr AppE (VarE r') [ ConE 'NumberV, VarE $ if fiIsInt then 'fromIntegral else 'fromRational , VarE fiName ]
          mkClause fi@FieldInfo{..} = Clause [VarP r', ConP fiConstructorName []] (NormalB $ mkBody fi) []
      in FunD 'recordValue $ mkClause <$> fis


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
    (valueType, isInt) <- case t of
      ConT n' -> do
        (isNum, isInt) <- (,) <$> isInstance ''Num [t] <*> isInstance ''Integral [t] :: Q (Bool, Bool)
        if
          | isNum -> pure ('NumberType, isInt)
          | t == ConT ''String -> pure ('StringType, False)
      _ -> fail $ "Expected simple type for field " ++ show n
    pure $ FieldInfo
      { fiName = n
      , fiConstructorName = mkName . pascal $ nameBase n
      , fiStringName = nameBase n
      , fiValueType = valueType
      , fiIsInt = isInt }
  
  pure
    [ deriveDataRecordField recordFieldTypeName recordFields
    , deriveInstanceTyped recordFieldTypeName recordFields
    , mkInstanceRecordField recordFields
    ]
  where
    recordFieldTypeName = mkName $ "RecordField" ++ nameBase r
    mkInstanceRecordField fis =
      let mkFromRecordId = 
            let sn' = LitP . TH.StringL
            in FunD 'fromRecordId $ 
              [ Clause [ sn' fiStringName ] (NormalB $ AppE (ConE 'Just) (ConE fiConstructorName)) [] | FieldInfo{..} <- fis ] ++ [ Clause [ WildP ] (NormalB (ConE 'Nothing)) [] ]
          mkToRecordId = FunD 'toRecordId [ Clause [ ConP fiConstructorName [] ] (NormalB . LitE $ TH.StringL fiStringName) [] | FieldInfo{..} <- fis ]
          mkRecordOf = TySynInstD ''RecordOf $ TySynEqn [ConT recordFieldTypeName] (ConT r)
          mkRecordValue =
            let mkBody FieldInfo{..}
                  | fiValueType == 'StringType = foldr AppE (VarE $ mkName "r") [ ConE 'StringV, VarE fiName ]
                  | fiValueType == 'NumberType = foldr AppE (VarE $ mkName "r") [ ConE 'NumberV, VarE $ if fiIsInt then 'fromIntegral else 'id , VarE fiName ]
                mkClause fi@FieldInfo{..} = Clause [VarP (mkName "r"), ConP fiConstructorName []] (NormalB $ mkBody fi) []
            in FunD 'recordValue $ mkClause <$> fis
      in InstanceD Nothing [] (AppT (ConT ''RecordField) $ ConT recordFieldTypeName) [ mkRecordOf, mkFromRecordId, mkToRecordId, mkRecordValue ]
