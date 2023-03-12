(set-option :auto-config false)
(set-option :model true)

(set-option :smt.mbqi false)

(define-sort Str () Int)
(declare-fun strLen (Str) Int)
(declare-fun subString (Str Int Int) Str)
(declare-fun concatString (Str Str) Str)
(define-sort Elt () Int)
(define-sort LSet () (Array Elt Bool))
(define-fun smt_set_emp () LSet ((as const LSet) false))
(define-fun smt_set_sng ((x Elt)) LSet (store ((as const LSet) false) x true))
(define-fun smt_set_mem ((x Elt) (s LSet)) Bool (select s x))
(define-fun smt_set_add ((s LSet) (x Elt)) LSet (store s x true))
(define-fun smt_set_cup ((s1 LSet) (s2 LSet)) LSet ((_ map or) s1 s2))
(define-fun smt_set_cap ((s1 LSet) (s2 LSet)) LSet ((_ map and) s1 s2))
(define-fun smt_set_com ((s LSet)) LSet ((_ map not) s))
(define-fun smt_set_dif ((s1 LSet) (s2 LSet)) LSet (smt_set_cap s1 (smt_set_com s2)))
(define-fun smt_set_sub ((s1 LSet) (s2 LSet)) Bool (= smt_set_emp (smt_set_dif s1 s2)))
(define-sort Map () (Array Elt Elt))
(define-fun smt_map_sel ((m Map) (k Elt)) Elt (select m k))
(define-fun smt_map_sto ((m Map) (k Elt) (v Elt)) Map (store m k v))
(define-fun smt_map_cup ((m1 Map) (m2 Map)) Map ((_ map (+ (Elt Elt) Elt)) m1 m2))
(define-fun smt_map_prj ((s LSet) (m Map)) Map ((_ map (ite (Bool Elt Elt) Elt)) s m ((as const (Array Elt Elt)) 0)))
(define-fun smt_map_to_set ((m Map)) LSet ((_ map (> (Elt Elt) Bool)) m ((as const (Array Elt Elt)) 0)))
(define-fun smt_map_max ((m1 Map) (m2 Map)) Map (lambda ((i Int)) (ite (> (select m1 i) (select m2 i)) (select m1 i) (select m2 i))))
(define-fun smt_map_min ((m1 Map) (m2 Map)) Map (lambda ((i Int)) (ite (< (select m1 i) (select m2 i)) (select m1 i) (select m2 i))))
(define-fun smt_map_shift ((n Int) (m Map)) Map (lambda ((i Int)) (select m (- i n))))
(define-fun smt_map_def ((v Elt)) Map ((as const (Map)) v))
(define-fun bool_to_int ((b Bool)) Int (ite b 1 0))
(define-fun Z3_OP_MUL ((x Int) (y Int)) Int (* x y))
(define-fun Z3_OP_DIV ((x Int) (y Int)) Int (div x y))
(declare-fun GHC.Generics.$36$fSingISourceUnpackednessNoSourceUnpackedness () Int)
(declare-fun papp7 () Int)
(declare-fun Data.Aeson.Types.FromJSON.genericParseJSON () Int)
(declare-fun GHC.Base.id () Int)
(declare-fun Data.Aeson.Types.Internal.TaggedObject () Int)
(declare-fun GHC.Types.KindRepTypeLitS () Int)
(declare-fun totalityError () Int)
(declare-fun Data.Aeson.Types.Generic.C$58$ProductSize () Int)
(declare-fun Data.ByteString.Internal.$36$WPS () Int)
(declare-fun GHC.Types.$91$$93$ () Int)
(declare-fun Data.Aeson.Types.Generic.C$58$AllNullary () Int)
(declare-fun GHC.Types.Word32Rep () Int)
(declare-fun GHC.Generics.InfixI () Int)
(declare-fun GHC.Real.divMod () Int)
(declare-fun GHC.Unicode.Surrogate () Int)
(declare-fun GHC.List.reverse () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fKeyValuePairvDList () Int)
(declare-fun InterfaceAdapters.Utils.Helper.SomeIOError () Int)
(declare-fun GHC.Float.$36$fShowFloat () Int)
(declare-fun Data.Text.Encoding.Error.DecodeError () Int)
(declare-fun GHC.Generics.SourceUnpack () Int)
(declare-fun Data.Aeson.Types.FromJSON.C$58$FromJSONKey () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fSumToJSONTYPEencarityfFalse () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fToJSONChar () Int)
(declare-fun GHC.List.scanl () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$fRecordFromJSONarityf () Int)
(declare-fun GHC.Classes.min () Int)
(declare-fun GHC.Int.I64$35$ () Int)
(declare-fun GHC.Types.C$35$ () Int)
(declare-fun Data.Aeson.Types.Generic.C$58$IsRecord () Int)
(declare-fun GHC.Types.$36$tcChar () Int)
(declare-fun GHC.Generics.Par1 () Int)
(declare-fun GHC.Real.$47$ () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fToJSON$91$$93$ () Int)
(declare-fun GHC.Float.FFExponent () Int)
(declare-fun GHC.Unicode.MathSymbol () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$fConsFromJSON$39$TYPEarityfTrue () Int)
(declare-fun GHC.Unicode.SpacingCombiningMark () Int)
(declare-fun GHC.Word.W32$35$ () Int)
(declare-fun GHC.Types.$58$ () Int)
(declare-fun GHC.Unicode.ConnectorPunctuation () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$fConstructorNamesM10 () Int)
(declare-fun tail () Int)
(declare-fun GHC.IO.Handle.Types.$36$WFileHandle () Int)
(declare-fun Data.Text.Internal.Lazy.Chunk () Int)
(declare-fun Data.ByteString.Lazy.Internal.Chunk () Int)
(declare-fun Data.Aeson.Types.ToJSON.C$58$ToJSON () Int)
(declare-fun GHC.Unicode.OtherSymbol () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$fFieldNamesk$58$$42$$58$ () Int)
(declare-fun isJust () Int)
(declare-fun Data.Time.Clock.Internal.UTCTime.UTCTime () Int)
(declare-fun GHC.Types.Word8Rep () Int)
(declare-fun InterfaceAdapters.Utils.Helper.MissingEnvError () Int)
(declare-fun InterfaceAdapters.IP.GeoIpAPI.$36$fToJSONForwardGeoData () Int)
(declare-fun GHC.Real.div () Int)
(declare-fun Control.Applicative.WrapMonad () Int)
(declare-fun GHC.Conc.Sync.ThreadId () Int)
(declare-fun GHC.Classes.$62$$61$ () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$fRecordFromJSON$39$arity$58$$42$$58$ () Int)
(declare-fun GHC.Generics.MetaCons () Int)
(declare-fun GHC.IO.Handle.Types.DuplexHandle () Int)
(declare-fun GHC.List.span () Int)
(declare-fun Data.Text.Internal.Lazy.$36$WChunk () Int)
(declare-fun InterfaceAdapters.IP.GeoIpAPI.$36$fGenericGeoIp () Int)
(declare-fun GHC.IO.Handle.Types.FileHandle () Int)
(declare-fun InterfaceAdapters.IP.GeoIpAPI.Locdata () Int)
(declare-fun Data.Aeson.Key.Key () Int)
(declare-fun GHC.Word.W8$35$ () Int)
(declare-fun InterfaceAdapters.IP.GeoCodeOpenCage.OpenCageLocdata () Int)
(declare-fun GHC.Tuple.$40$$44$$41$ () Int)
(declare-fun GHC.Real.$94$ () Int)
(declare-fun GHC.Classes.$61$$61$ () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fSumToJSON$39$TYPETwoElemArrayValuearityM1 () Int)
(declare-fun GHC.List.$33$$33$ () Int)
(declare-fun GHC.Base.$36$ () Int)
(declare-fun GHC.Generics.$36$fSingIFixityIPrefixI () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fTaggedObject$39$TYPEencpairsarityfTrue () Int)
(declare-fun GHC.Types.W$35$ () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WBool () Int)
(declare-fun GHC.List.last () Int)
(declare-fun Data.Aeson.Types.Internal.Object () Int)
(declare-fun GHC.Show.$36$fShow$91$$93$ () Int)
(declare-fun Data.Aeson.Types.Internal.Error () Int)
(declare-fun GHC.List.zipWith () Int)
(declare-fun papp5 () Int)
(declare-fun GHC.Types.Word16Rep () Int)
(declare-fun snd () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$fFromJSONFloat () Int)
(declare-fun InterfaceAdapters.IP.GeoIpAPI.ForwardGeoData () Int)
(declare-fun GHC.Base.$43$$43$ () Int)
(declare-fun GHC.Maybe.Just () Int)
(declare-fun x_Tuple22 () Int)
(declare-fun GHC.Unicode.Space () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WNumber () Int)
(declare-fun GHC.Generics.C$58$Generic () Int)
(declare-fun GHC.Classes.$36$fOrdInt () Int)
(declare-fun GHC.TypeLits.C$58$KnownSymbol () Int)
(declare-fun GHC.Base.$62$$62$$61$ () Int)
(declare-fun GHC.Unicode.TitlecaseLetter () Int)
(declare-fun Data.Aeson.KeyMap.KeyMap () Int)
(declare-fun Data.Aeson.Types.Internal.TwoElemArray () Int)
(declare-fun GHC.Unicode.ClosePunctuation () Int)
(declare-fun GHC.Real.fromIntegral () Int)
(declare-fun GHC.IO.Exception.IOError () Int)
(declare-fun GHC.List.tail () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fToJSONInt () Int)
(declare-fun GHC.IO.MaskedUninterruptible () Int)
(declare-fun GHC.Types.Module () Int)
(declare-fun GHC.Stack.Types.FreezeCallStack () Int)
(declare-fun Data.Aeson.Types.Generic.$36$fAllNullaryM1allNullary () Int)
(declare-fun GHC.Types.TrNameD () Int)
(declare-fun Data.Text.Lazy.pack () Int)
(declare-fun GHC.Types.Word64Rep () Int)
(declare-fun GHC.Types.TyCon () Int)
(declare-fun GHC.Show.showParen () Int)
(declare-fun fromJust () Int)
(declare-fun InterfaceAdapters.IP.GeoIpAPI.$36$fFromJSONForwardGeoData () Int)
(declare-fun GHC.Types.UnliftedRep () Int)
(declare-fun Data.Aeson.Types.FromJSON.FromJSONKeyCoerce () Int)
(declare-fun Data.Aeson.Types.Internal.Array () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$dmtoEncodingList () Int)
(declare-fun GHC.Exception.Type.SomeException () Int)
(declare-fun GHC.Types.$36$tcFloat () Int)
(declare-fun GHC.Unicode.FinalQuote () Int)
(declare-fun GHC.IO.catch () Int)
(declare-fun GHC.Generics.PrefixI () Int)
(declare-fun GHC.Unicode.PrivateUse () Int)
(declare-fun InterfaceAdapters.IP.GeoIpAPI.$36$fFromJSONLocdata () Int)
(declare-fun Data.Foldable.length () Int)
(declare-fun GHC.Types.IntRep () Int)
(declare-fun GHC.Generics.$36$fConstructorMetaMetaCons () Int)
(declare-fun GHC.Types.DoubleRep () Int)
(declare-fun GHC.Float.FFGeneric () Int)
(declare-fun GHC.Classes.not () Int)
(declare-fun GHC.List.iterate () Int)
(declare-fun Data.Text.Internal.Text () Int)
(declare-fun GHC.Unicode.OtherLetter () Int)
(declare-fun InterfaceAdapters.IP.GeoIpAPI.$36$fGenericForwardGeoData () Int)
(declare-fun GHC.Generics.Infix () Int)
(declare-fun GHC.Generics.Rec1 () Int)
(declare-fun Data.ByteString.Internal.PS () Int)
(declare-fun GHC.Types.Int8Rep () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$fGFromJSONarityK1 () Int)
(declare-fun GHC.Generics.$36$fSelectorMetaMetaSel () Int)
(declare-fun GHC.Show.showsPrec () Int)
(declare-fun Data.Aeson.Types.Internal.Index () Int)
(declare-fun addrLen () Int)
(declare-fun GHC.Real.$36$W$58$$37$ () Int)
(declare-fun GHC.Unicode.EnclosingMark () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$fConsFromJSONarityf () Int)
(declare-fun GHC.Generics.DecidedStrict () Int)
(declare-fun GHC.List.splitAt () Int)
(declare-fun GHC.Generics.$36$fSingISourceStrictnessNoSourceStrictness () Int)
(declare-fun GHC.Types.KindRepFun () Int)
(declare-fun GHC.Types.WordRep () Int)
(declare-fun papp3 () Int)
(declare-fun GHC.Show.$36$dmshow () Int)
(declare-fun GHC.List.scanl1 () Int)
(declare-fun GHC.Real.toInteger () Int)
(declare-fun Data.Aeson.Types.Generic.$36$fIsRecordM1isRecord () Int)
(declare-fun InterfaceAdapters.IP.GeoIpAPI.$36$fFromJSONGeoIp () Int)
(declare-fun Data.Aeson.Types.Internal.defaultOptions () Int)
(declare-fun Data.DList.Internal.$36$fMonoidDList () Int)
(declare-fun Data.Foldable.null () Int)
(declare-fun Data.Aeson.Types.ToJSON.C$58$ToJSONKey () Int)
(declare-fun Data.Aeson.Types.Internal.Bool () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fSumToJSON$39$TYPEObjectWithSingleFieldencarityM1 () Int)
(declare-fun Data.Text.pack () Int)
(declare-fun Data.Aeson.Types.Generic.$36$fIsRecordK1True () Int)
(declare-fun GHC.Classes.$38$$38$ () Int)
(declare-fun GHC.Unicode.UppercaseLetter () Int)
(declare-fun GHC.Classes.$124$$124$ () Int)
(declare-fun GHC.List.dropWhile () Int)
(declare-fun InterfaceAdapters.Utils.JSONHelper.getJSON () Int)
(declare-fun GHC.Exception.Type.$36$fExceptionSomeException () Int)
(declare-fun InterfaceAdapters.IP.GeoCodeOpenCage.OpenCageResultData () Int)
(declare-fun Data.Scientific.Scientific () Int)
(declare-fun GHC.Show.$36$fShowInt () Int)
(declare-fun Data.Aeson.Types.Internal.String () Int)
(declare-fun GHC.Enum.C$58$Bounded () Int)
(declare-fun GHC.Unicode.OtherPunctuation () Int)
(declare-fun GHC.List.init () Int)
(declare-fun GHC.List.scanr1 () Int)
(declare-fun GHC.Types.True () Bool)
(declare-fun Data.Aeson.Types.FromJSON.$36$fGFromJSON$39$arityM1 () Int)
(declare-fun GHC.CString.unpackCString$35$ () Int)
(declare-fun GHC.Generics.$36$fSingIMaybeJust () Int)
(declare-fun InterfaceAdapters.IP.GeoIpAPI.GeoIp () Int)
(declare-fun GHC.Unicode.LineSeparator () Int)
(declare-fun GHC.List.break () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$fFromTaggedObject$39$TYPEarityfTrue () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fGToJSON$39$ValuearityK1 () Int)
(declare-fun GHC.Generics.MetaData () Int)
(declare-fun GHC.Base.$62$$62$ () Int)
(declare-fun GHC.Types.$36$WKindRepVar () Int)
(declare-fun GHC.Stack.Types.EmptyCallStack () Int)
(declare-fun x_Tuple33 () Int)
(declare-fun GHC.Unicode.CurrencySymbol () Int)
(declare-fun Data.Either.Right () Int)
(declare-fun Data.Aeson.Types.FromJSON.C$58$FromJSON () Int)
(declare-fun Data.ByteString.Lazy.empty () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fConsToJSON$39$TYPEencarityfTrue () Int)
(declare-fun GHC.IO.MaskedInterruptible () Int)
(declare-fun GHC.Show.C$58$Show () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fWriteProductaritya () Int)
(declare-fun InterfaceAdapters.IP.GeoIpAPI.$36$fToJSONLocdata () Int)
(declare-fun GHC.Generics.$36$fSingISymbola () Int)
(declare-fun GHC.Unicode.ModifierLetter () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WIndex () Int)
(declare-fun GHC.Show.showCommaSpace () Int)
(declare-fun GHC.Generics.$36$fSingIDecidedStrictnessDecidedLazy () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$dmtoEncoding () Int)
(declare-fun GHC.Generics.$36$fDatatypeMetaMetaData () Int)
(declare-fun cast_as () Int)
(declare-fun GHC.Ptr.Ptr () Int)
(declare-fun Data.Text.Internal.$36$WText () Int)
(declare-fun GHC.Types.KindRepApp () Int)
(declare-fun Network.HTTP.Conduit.simpleHttp () Int)
(declare-fun Data.Aeson.Types.Generic.$36$fProductSize$58$$42$$58$ () Int)
(declare-fun GHC.List.head () Int)
(declare-fun Data.ByteString.Lazy.Internal.$36$WChunk () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$dmparseJSON () Int)
(declare-fun Data.Aeson.Types.Internal.Options () Int)
(declare-fun cast_as_int () Int)
(declare-fun GHC.Num.$43$ () Int)
(declare-fun GHC.Types.F$35$ () Int)
(declare-fun GHC.Base.$36$fMonadIO () Int)
(declare-fun GHC.Generics.DecidedLazy () Int)
(declare-fun GHC.Unicode.ParagraphSeparator () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fGToJSON$39$encarityM11 () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WObject () Int)
(declare-fun len () Int)
(declare-fun Data.Aeson.Types.Internal.Key () Int)
(declare-fun GHC.Generics.$36$fSingIBoolFalse () Int)
(declare-fun GHC.Unicode.LetterNumber () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$fFromUntaggedValuearityM10 () Int)
(declare-fun GHC.Types.Int64Rep () Int)
(declare-fun GHC.Types.LT () Int)
(declare-fun InterfaceAdapters.Utils.Helper.key () Int)
(declare-fun GHC.Tuple.$40$$44$$44$$41$ () Int)
(declare-fun papp1 () Int)
(declare-fun GHC.Unicode.LowercaseLetter () Int)
(declare-fun GHC.List.drop () Int)
(declare-fun Data.Aeson.Types.Generic.$36$fProductSizeM1 () Int)
(declare-fun GHC.Types.AddrRep () Int)
(declare-fun papp6 () Int)
(declare-fun GHC.Real.mod () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$fConsFromJSON$39$TYPEarityM1True () Int)
(declare-fun GHC.Generics.$58$$42$$58$ () Int)
(declare-fun GHC.Classes.C$58$IP () Int)
(declare-fun Data.Either.Left () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fGToJSON$39$Valuearity$58$$42$$58$ () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fConsToJSON$39$TYPEencarityM1True () Int)
(declare-fun GHC.Classes.C$58$Eq () Int)
(declare-fun GHC.Unicode.OtherNumber () Int)
(declare-fun Data.Functor.Const.Const () Int)
(declare-fun GHC.List.zip () Int)
(declare-fun liquid_internal_this () Int)
(declare-fun GHC.Real.fromRational () Int)
(declare-fun GHC.Classes.$62$ () Int)
(declare-fun GHC.Types.KindRepTyConApp () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$fFromJSONInt () Int)
(declare-fun GHC.Tuple.$40$$41$ () Int)
(declare-fun lqdc$35$$35$$36$select$35$$35$GHC.Maybe.Just$35$$35$1 () Int)
(declare-fun GHC.Types.KindRepVar () Int)
(declare-fun GHC.Types.I$35$ () Int)
(declare-fun InterfaceAdapters.IP.GeoCodeOpenCage.OpenCageForwardGeoData () Int)
(declare-fun GHC.IO.Unmasked () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$dmtoJSONList () Int)
(declare-fun GHC.Generics.K1 () Int)
(declare-fun GHC.Types.KindRepTypeLitD () Int)
(declare-fun Data.ByteString.Lazy.Internal.Empty () Int)
(declare-fun x_Tuple31 () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fRecordToPairsencpairsarity$58$$42$$58$ () Int)
(declare-fun GHC.Unicode.OpenPunctuation () Int)
(declare-fun Data.Text.Internal.Lazy.Empty () Int)
(declare-fun Data.Aeson.Types.Generic.$36$fAllNullary$58$$42$$58$False () Int)
(declare-fun InterfaceAdapters.IP.GeoIpAPI.$36$fShowLocdata () Int)
(declare-fun GHC.Num.Integer.IS () Int)
(declare-fun GHC.Unicode.Format () Int)
(declare-fun GHC.Types.FloatRep () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$fParseSumTYPEarityfFalse () Int)
(declare-fun GHC.Arr.$36$WArray () Int)
(declare-fun Data.Vector.Vector () Int)
(declare-fun GHC.Real.quot () Int)
(declare-fun Data.Vector.$36$WVector () Int)
(declare-fun Data.Tuple.fst () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WArray () Int)
(declare-fun Data.Aeson.Types.Internal.Null () Int)
(declare-fun GHC.Word.W64$35$ () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$fFromJSON$91$$93$ () Int)
(declare-fun GHC.Generics.NoSourceStrictness () Int)
(declare-fun GHC.Show.$36$dmshowList () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$dmtoJSON () Int)
(declare-fun GHC.Types.$36$tcInt () Int)
(declare-fun InterfaceAdapters.IP.GeoIpAPI.$36$fShowForwardGeoData () Int)
(declare-fun GHC.Types.LiftedRep () Int)
(declare-fun Data.Tuple.snd () Int)
(declare-fun GHC.Generics.SourceStrict () Int)
(declare-fun GHC.Types.EQ () Int)
(declare-fun Control.Monad.IO.Class.$36$fMonadIOIO () Int)
(declare-fun GHC.Generics.SourceLazy () Int)
(declare-fun Data.Aeson.Types.Internal.ObjectWithSingleField () Int)
(declare-fun GHC.Real.$58$$37$ () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$fGFromJSONarityM10 () Int)
(declare-fun GHC.Types.VecRep () Int)
(declare-fun fst () Int)
(declare-fun InterfaceAdapters.Utils.Helper.EmptyKeyError () Int)
(declare-fun GHC.List.takeWhile () Int)
(declare-fun GHC.Show.showString () Int)
(declare-fun GHC.Num.negate () Int)
(declare-fun GHC.Generics.DecidedUnpack () Int)
(declare-fun GHC.Unicode.ModifierSymbol () Int)
(declare-fun GHC.Num.$45$ () Int)
(declare-fun GHC.Real.rem () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fGToJSON$39$encarityM1 () Int)
(declare-fun Data.Aeson.Types.Generic.$36$fAllNullaryK1False () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fFromPairsValueDList () Int)
(declare-fun GHC.Types.krep$36$$42$ () Int)
(declare-fun GHC.Classes.max () Int)
(declare-fun Data.Aeson.Encoding.Internal.Empty () Int)
(declare-fun GHC.Generics.MetaSel () Int)
(declare-fun GHC.List.scanr () Int)
(declare-fun GHC.Real.recip () Int)
(declare-fun GHC.Generics.NoSourceUnpackedness () Int)
(declare-fun autolen () Int)
(declare-fun GHC.Float.FFFixed () Int)
(declare-fun GHC.Unicode.Control () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fRecordToPairsencpairsarityM11 () Int)
(declare-fun GHC.List.replicate () Int)
(declare-fun GHC.Base.. () Int)
(declare-fun GHC.List.take () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$fRecordFromJSON$39$arityM11 () Int)
(declare-fun InterfaceAdapters.IP.GeoIpAPI.$36$fToJSONGeoIp () Int)
(declare-fun GHC.Types.Int16Rep () Int)
(declare-fun papp4 () Int)
(declare-fun GHC.Base.return () Int)
(declare-fun GHC.Types.$36$WKindRepTYPE () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fTaggedObjectencarityM1 () Int)
(declare-fun Data.Aeson.Types.Internal.Success () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WString () Int)
(declare-fun GHC.Stack.Types.PushCallStack () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$dmparseJSONList () Int)
(declare-fun GHC.Real.quotRem () Int)
(declare-fun GHC.Classes.$60$$61$ () Int)
(declare-fun GHC.Types.Eq$35$ () Int)
(declare-fun GHC.Num.fromInteger () Int)
(declare-fun GHC.Types.KindRepTYPE () Int)
(declare-fun GHC.Classes.$60$ () Int)
(declare-fun Data.Text.Lazy.Encoding.encodeUtf8 () Int)
(declare-fun Data.Aeson.Types.Internal.UntaggedValue () Int)
(declare-fun GHC.Types.SumRep () Int)
(declare-fun GHC.Unicode.NotAssigned () Int)
(declare-fun GHC.List.filter () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fFromStringValue () Int)
(declare-fun x_Tuple21 () Int)
(declare-fun Data.Aeson.Types.Internal.DotNetTime () Int)
(declare-fun GHC.List.repeat () Int)
(declare-fun Data.Text.Encoding.Error.EncodeError () Int)
(declare-fun head () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$fFromTaggedObjectarityM1 () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fToJSONFloat () Int)
(declare-fun GHC.Maybe.Nothing () Int)
(declare-fun GHC.Generics.Comp1 () Int)
(declare-fun GHC.Unicode.DecimalNumber () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fSumToJSON$39$TYPEUntaggedValueencarityM10 () Int)
(declare-fun GHC.Types.D$35$ () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fConsToJSONencarityf () Int)
(declare-fun GHC.Generics.SourceNoUnpack () Int)
(declare-fun GHC.Num.Integer.IP () Int)
(declare-fun Control.Applicative.WrapArrow () Int)
(declare-fun GHC.Base.map () Int)
(declare-fun GHC.Generics.C$58$SingI () Int)
(declare-fun x_Tuple32 () Int)
(declare-fun Data.Aeson.Types.Internal.Number () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fWriteProductarity$58$$42$$58$ () Int)
(declare-fun Data.Aeson.Types.Generic.$36$fIsRecord$58$$42$$58$isRecord () Int)
(declare-fun GHC.Generics.$36$fSingIBoolTrue () Int)
(declare-fun GHC.Err.error () Int)
(declare-fun GHC.Types.TupleRep () Int)
(declare-fun GHC.Classes.compare () Int)
(declare-fun GHC.Unicode.NonSpacingMark () Int)
(declare-fun GHC.Types.TrNameS () Int)
(declare-fun GHC.Types.$36$tc$91$$93$ () Int)
(declare-fun GHC.Num.Integer.IN () Int)
(declare-fun GHC.Generics.Prefix () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$fFromJSONChar () Int)
(declare-fun System.IO.putStrLn () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$fFieldNameskM1 () Int)
(declare-fun GHC.Num.$42$ () Int)
(declare-fun InterfaceAdapters.IP.GeoIpAPI.$36$fGenericLocdata () Int)
(declare-fun GHC.Types.False () Bool)
(declare-fun GHC.Arr.Array () Int)
(declare-fun GHC.Show.$36$fShowChar () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$fFromPairarityM1 () Int)
(declare-fun GHC.Generics.M1 () Int)
(declare-fun Data.Scientific.$36$WScientific () Int)
(declare-fun Control.Applicative.ZipList () Int)
(declare-fun GHC.Unicode.InitialQuote () Int)
(declare-fun InterfaceAdapters.IP.GeoIpAPI.$36$fShowGeoIp () Int)
(declare-fun papp2 () Int)
(declare-fun GHC.Classes.$47$$61$ () Int)
(declare-fun GHC.List.cycle () Int)
(declare-fun Data.String.fromString () Int)
(declare-fun GHC.IO.Handle.Types.$36$WDuplexHandle () Int)
(declare-fun GHC.Types.GT () Int)
(declare-fun GHC.Types.Int32Rep () Int)
(declare-fun GHC.Unicode.DashPunctuation () Int)
(declare-fun apply$35$$35$21 (Int (_ BitVec 32)) Bool)
(declare-fun apply$35$$35$1 (Int Int) Bool)
(declare-fun apply$35$$35$4 (Int Int) (_ BitVec 32))
(declare-fun apply$35$$35$7 (Int Bool) Real)
(declare-fun apply$35$$35$24 (Int (_ BitVec 32)) (_ BitVec 32))
(declare-fun apply$35$$35$12 (Int Real) Real)
(declare-fun apply$35$$35$23 (Int (_ BitVec 32)) Str)
(declare-fun apply$35$$35$16 (Int Str) Bool)
(declare-fun apply$35$$35$5 (Int Bool) Int)
(declare-fun apply$35$$35$11 (Int Real) Bool)
(declare-fun apply$35$$35$18 (Int Str) Str)
(declare-fun apply$35$$35$19 (Int Str) (_ BitVec 32))
(declare-fun apply$35$$35$2 (Int Int) Real)
(declare-fun apply$35$$35$13 (Int Real) Str)
(declare-fun apply$35$$35$20 (Int (_ BitVec 32)) Int)
(declare-fun apply$35$$35$15 (Int Str) Int)
(declare-fun apply$35$$35$0 (Int Int) Int)
(declare-fun apply$35$$35$8 (Int Bool) Str)
(declare-fun apply$35$$35$9 (Int Bool) (_ BitVec 32))
(declare-fun apply$35$$35$22 (Int (_ BitVec 32)) Real)
(declare-fun apply$35$$35$14 (Int Real) (_ BitVec 32))
(declare-fun apply$35$$35$17 (Int Str) Real)
(declare-fun apply$35$$35$3 (Int Int) Str)
(declare-fun apply$35$$35$6 (Int Bool) Bool)
(declare-fun apply$35$$35$10 (Int Real) Int)
(declare-fun coerce$35$$35$21 ((_ BitVec 32)) Bool)
(declare-fun coerce$35$$35$1 (Int) Bool)
(declare-fun coerce$35$$35$4 (Int) (_ BitVec 32))
(declare-fun coerce$35$$35$7 (Bool) Real)
(declare-fun coerce$35$$35$24 ((_ BitVec 32)) (_ BitVec 32))
(declare-fun coerce$35$$35$12 (Real) Real)
(declare-fun coerce$35$$35$23 ((_ BitVec 32)) Str)
(declare-fun coerce$35$$35$16 (Str) Bool)
(declare-fun coerce$35$$35$5 (Bool) Int)
(declare-fun coerce$35$$35$11 (Real) Bool)
(declare-fun coerce$35$$35$18 (Str) Str)
(declare-fun coerce$35$$35$19 (Str) (_ BitVec 32))
(declare-fun coerce$35$$35$2 (Int) Real)
(declare-fun coerce$35$$35$13 (Real) Str)
(declare-fun coerce$35$$35$20 ((_ BitVec 32)) Int)
(declare-fun coerce$35$$35$15 (Str) Int)
(declare-fun coerce$35$$35$0 (Int) Int)
(declare-fun coerce$35$$35$8 (Bool) Str)
(declare-fun coerce$35$$35$9 (Bool) (_ BitVec 32))
(declare-fun coerce$35$$35$22 ((_ BitVec 32)) Real)
(declare-fun coerce$35$$35$14 (Real) (_ BitVec 32))
(declare-fun coerce$35$$35$17 (Str) Real)
(declare-fun coerce$35$$35$3 (Int) Str)
(declare-fun coerce$35$$35$6 (Bool) Bool)
(declare-fun coerce$35$$35$10 (Real) Int)
(declare-fun smt_lambda$35$$35$21 ((_ BitVec 32) Bool) Int)
(declare-fun smt_lambda$35$$35$1 (Int Bool) Int)
(declare-fun smt_lambda$35$$35$4 (Int (_ BitVec 32)) Int)
(declare-fun smt_lambda$35$$35$7 (Bool Real) Int)
(declare-fun smt_lambda$35$$35$24 ((_ BitVec 32) (_ BitVec 32)) Int)
(declare-fun smt_lambda$35$$35$12 (Real Real) Int)
(declare-fun smt_lambda$35$$35$23 ((_ BitVec 32) Str) Int)
(declare-fun smt_lambda$35$$35$16 (Str Bool) Int)
(declare-fun smt_lambda$35$$35$5 (Bool Int) Int)
(declare-fun smt_lambda$35$$35$11 (Real Bool) Int)
(declare-fun smt_lambda$35$$35$18 (Str Str) Int)
(declare-fun smt_lambda$35$$35$19 (Str (_ BitVec 32)) Int)
(declare-fun smt_lambda$35$$35$2 (Int Real) Int)
(declare-fun smt_lambda$35$$35$13 (Real Str) Int)
(declare-fun smt_lambda$35$$35$20 ((_ BitVec 32) Int) Int)
(declare-fun smt_lambda$35$$35$15 (Str Int) Int)
(declare-fun smt_lambda$35$$35$0 (Int Int) Int)
(declare-fun smt_lambda$35$$35$8 (Bool Str) Int)
(declare-fun smt_lambda$35$$35$9 (Bool (_ BitVec 32)) Int)
(declare-fun smt_lambda$35$$35$22 ((_ BitVec 32) Real) Int)
(declare-fun smt_lambda$35$$35$14 (Real (_ BitVec 32)) Int)
(declare-fun smt_lambda$35$$35$17 (Str Real) Int)
(declare-fun smt_lambda$35$$35$3 (Int Str) Int)
(declare-fun smt_lambda$35$$35$6 (Bool Bool) Int)
(declare-fun smt_lambda$35$$35$10 (Real Int) Int)
(declare-fun lam_arg$35$$35$1$35$$35$5 () Bool)
(declare-fun lam_arg$35$$35$2$35$$35$5 () Bool)
(declare-fun lam_arg$35$$35$3$35$$35$5 () Bool)
(declare-fun lam_arg$35$$35$4$35$$35$5 () Bool)
(declare-fun lam_arg$35$$35$5$35$$35$5 () Bool)
(declare-fun lam_arg$35$$35$6$35$$35$5 () Bool)
(declare-fun lam_arg$35$$35$7$35$$35$5 () Bool)
(declare-fun lam_arg$35$$35$1$35$$35$20 () (_ BitVec 32))
(declare-fun lam_arg$35$$35$2$35$$35$20 () (_ BitVec 32))
(declare-fun lam_arg$35$$35$3$35$$35$20 () (_ BitVec 32))
(declare-fun lam_arg$35$$35$4$35$$35$20 () (_ BitVec 32))
(declare-fun lam_arg$35$$35$5$35$$35$20 () (_ BitVec 32))
(declare-fun lam_arg$35$$35$6$35$$35$20 () (_ BitVec 32))
(declare-fun lam_arg$35$$35$7$35$$35$20 () (_ BitVec 32))
(declare-fun lam_arg$35$$35$1$35$$35$15 () Str)
(declare-fun lam_arg$35$$35$2$35$$35$15 () Str)
(declare-fun lam_arg$35$$35$3$35$$35$15 () Str)
(declare-fun lam_arg$35$$35$4$35$$35$15 () Str)
(declare-fun lam_arg$35$$35$5$35$$35$15 () Str)
(declare-fun lam_arg$35$$35$6$35$$35$15 () Str)
(declare-fun lam_arg$35$$35$7$35$$35$15 () Str)
(declare-fun lam_arg$35$$35$1$35$$35$0 () Int)
(declare-fun lam_arg$35$$35$2$35$$35$0 () Int)
(declare-fun lam_arg$35$$35$3$35$$35$0 () Int)
(declare-fun lam_arg$35$$35$4$35$$35$0 () Int)
(declare-fun lam_arg$35$$35$5$35$$35$0 () Int)
(declare-fun lam_arg$35$$35$6$35$$35$0 () Int)
(declare-fun lam_arg$35$$35$7$35$$35$0 () Int)
(declare-fun lam_arg$35$$35$1$35$$35$10 () Real)
(declare-fun lam_arg$35$$35$2$35$$35$10 () Real)
(declare-fun lam_arg$35$$35$3$35$$35$10 () Real)
(declare-fun lam_arg$35$$35$4$35$$35$10 () Real)
(declare-fun lam_arg$35$$35$5$35$$35$10 () Real)
(declare-fun lam_arg$35$$35$6$35$$35$10 () Real)
(declare-fun lam_arg$35$$35$7$35$$35$10 () Real)




(assert (distinct GHC.Types.Int32Rep GHC.Types.Int16Rep GHC.Types.LiftedRep GHC.Types.FloatRep GHC.Types.AddrRep GHC.Types.Int64Rep GHC.Types.WordRep GHC.Types.Int8Rep GHC.Types.DoubleRep GHC.Types.IntRep GHC.Types.UnliftedRep GHC.Types.Word64Rep GHC.Types.Word16Rep GHC.Types.Word8Rep GHC.Types.Word32Rep))

(assert (distinct GHC.Float.FFFixed GHC.Float.FFGeneric GHC.Float.FFExponent))


(assert (distinct GHC.Types.GT GHC.Types.EQ GHC.Types.LT))

(assert (distinct GHC.Generics.SourceLazy GHC.Generics.SourceStrict GHC.Generics.NoSourceStrictness))






(assert (distinct Data.Aeson.Types.Internal.UntaggedValue Data.Aeson.Types.Internal.ObjectWithSingleField Data.Aeson.Types.Internal.TwoElemArray))


(assert (distinct GHC.Generics.SourceNoUnpack GHC.Generics.NoSourceUnpackedness GHC.Generics.SourceUnpack))
(assert (distinct GHC.Types.False GHC.Types.True))


(assert (distinct GHC.IO.Unmasked GHC.IO.MaskedInterruptible GHC.IO.MaskedUninterruptible))
(assert (distinct GHC.Generics.DecidedUnpack GHC.Generics.DecidedLazy GHC.Generics.DecidedStrict))
(assert (distinct GHC.Unicode.DashPunctuation GHC.Unicode.InitialQuote GHC.Unicode.NonSpacingMark GHC.Unicode.DecimalNumber GHC.Unicode.NotAssigned GHC.Unicode.Control GHC.Unicode.ModifierSymbol GHC.Unicode.Format GHC.Unicode.OpenPunctuation GHC.Unicode.OtherNumber GHC.Unicode.LowercaseLetter GHC.Unicode.LetterNumber GHC.Unicode.ParagraphSeparator GHC.Unicode.ModifierLetter GHC.Unicode.CurrencySymbol GHC.Unicode.LineSeparator GHC.Unicode.OtherPunctuation GHC.Unicode.UppercaseLetter GHC.Unicode.EnclosingMark GHC.Unicode.OtherLetter GHC.Unicode.PrivateUse GHC.Unicode.FinalQuote GHC.Unicode.ClosePunctuation GHC.Unicode.TitlecaseLetter GHC.Unicode.Space GHC.Unicode.OtherSymbol GHC.Unicode.ConnectorPunctuation GHC.Unicode.SpacingCombiningMark GHC.Unicode.MathSymbol GHC.Unicode.Surrogate))


(push 1)
(push 1)
(pop 1)
(pop 1)
(exit)
