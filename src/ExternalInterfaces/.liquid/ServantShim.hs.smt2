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
(declare-fun papp7 () Int)
(declare-fun GHC.Base.id () Int)
(declare-fun totalityError () Int)
(declare-fun Data.ByteString.Internal.$36$WPS () Int)
(declare-fun GHC.Types.$91$$93$ () Int)
(declare-fun Data.HashMap.Internal.$36$WCollision () Int)
(declare-fun GHC.Base.$36$fFunctorIO () Int)
(declare-fun Network.Wai.defaultRequest4 () Int)
(declare-fun Control.Concurrent.QSem.QSem () Int)
(declare-fun GHC.Real.divMod () Int)
(declare-fun Network.Wai.Internal.ResponseStream () Int)
(declare-fun GHC.List.reverse () Int)
(declare-fun Data.Semigroup.Internal.Sum () Int)
(declare-fun Control.Concurrent.Chan.Chan () Int)
(declare-fun Data.Text.Encoding.Error.DecodeError () Int)
(declare-fun Data.CaseInsensitive.Internal.$36$fIsStringCI () Int)
(declare-fun GHC.List.scanl () Int)
(declare-fun GHC.Classes.min () Int)
(declare-fun GHC.Int.I64$35$ () Int)
(declare-fun GHC.Types.C$35$ () Int)
(declare-fun GHC.Real.$47$ () Int)
(declare-fun Control.Lens.Setter.$63$$126$ () Int)
(declare-fun Network.HTTP.Types.Version.HttpVersion () Int)
(declare-fun Network.HTTP.Types.Method.DELETE () Int)
(declare-fun GHC.OverloadedLabels.C$58$IsLabel () Int)
(declare-fun Foreign.C.Types.CInt () Int)
(declare-fun Network.HTTP.Types.Method.HEAD () Int)
(declare-fun GHC.Word.W32$35$ () Int)
(declare-fun Data.Complex.$58$$43$ () Int)
(declare-fun GHC.Types.$58$ () Int)
(declare-fun Data.Semigroup.Internal.Alt () Int)
(declare-fun tail () Int)
(declare-fun GHC.IO.Handle.Types.$36$WFileHandle () Int)
(declare-fun Data.Text.Internal.Lazy.Chunk () Int)
(declare-fun Data.ByteString.Lazy.Internal.Chunk () Int)
(declare-fun System.Posix.Types.Fd () Int)
(declare-fun GHC.Base.mempty () Int)
(declare-fun Control.Concurrent.QSemN.QSemN () Int)
(declare-fun isJust () Int)
(declare-fun Data.ByteString.Lazy.toStrict () Int)
(declare-fun GHC.IO.Exception.ResourceVanished () Int)
(declare-fun GHC.IO.Exception.ExitSuccess () Int)
(declare-fun Data.Semigroup.Internal.Dual () Int)
(declare-fun GHC.ExecutionStack.Internal.SrcLoc () Int)
(declare-fun GHC.Real.div () Int)
(declare-fun AWSLambda.Events.APIGateway.$36$WAPIGatewayProxyRequest () Int)
(declare-fun Data.Functor.$60$$36$$62$ () Int)
(declare-fun Data.Function.$38$ () Int)
(declare-fun GHC.Conc.Sync.ThreadId () Int)
(declare-fun GHC.Classes.$62$$61$ () Int)
(declare-fun GHC.IORef.IORef () Int)
(declare-fun GHC.IO.Exception.UnsatisfiedConstraints () Int)
(declare-fun GHC.IO.Handle.Types.DuplexHandle () Int)
(declare-fun GHC.List.span () Int)
(declare-fun Data.Text.Internal.Lazy.$36$WChunk () Int)
(declare-fun GHC.IO.Handle.Types.FileHandle () Int)
(declare-fun Network.HTTP.Types.URI.renderQuery () Int)
(declare-fun GHC.Word.W8$35$ () Int)
(declare-fun GHC.Tuple.$40$$44$$41$ () Int)
(declare-fun Data.Semigroup.Internal.Product () Int)
(declare-fun GHC.Real.$94$ () Int)
(declare-fun GHC.IO.IOMode.WriteMode () Int)
(declare-fun GHC.Classes.$61$$61$ () Int)
(declare-fun Network.Wai.Internal.ResponseRaw () Int)
(declare-fun Data.Typeable.Internal.$36$WSomeTypeRep () Int)
(declare-fun GHC.List.$33$$33$ () Int)
(declare-fun GHC.Base.$36$ () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WBool () Int)
(declare-fun GHC.List.last () Int)
(declare-fun GHC.TypeNats.SomeNat () Int)
(declare-fun Data.Aeson.Types.Internal.Object () Int)
(declare-fun GHC.IO.Exception.ExitFailure () Int)
(declare-fun GHC.List.zipWith () Int)
(declare-fun Control.Concurrent.Chan.$36$WChan () Int)
(declare-fun papp5 () Int)
(declare-fun GHC.IORef.readIORef () Int)
(declare-fun snd () Int)
(declare-fun GHC.IO.Exception.UnsupportedOperation () Int)
(declare-fun Network.HTTP.Types.Header.$36$WByteRangeFromTo () Int)
(declare-fun GHC.Base.$43$$43$ () Int)
(declare-fun GHC.Maybe.Just () Int)
(declare-fun x_Tuple22 () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WNumber () Int)
(declare-fun GHC.Base.pure () Int)
(declare-fun Network.HTTP.Types.Version.http11 () Int)
(declare-fun GHC.TypeLits.C$58$KnownSymbol () Int)
(declare-fun GHC.Base.$62$$62$$61$ () Int)
(declare-fun GHC.Real.fromIntegral () Int)
(declare-fun GHC.IO.Exception.IOError () Int)
(declare-fun GHC.IORef.writeIORef () Int)
(declare-fun GHC.List.tail () Int)
(declare-fun GHC.IO.MaskedUninterruptible () Int)
(declare-fun Network.HTTP.Types.Status.Status () Int)
(declare-fun Data.Complex.$36$W$58$$43$ () Int)
(declare-fun Data.Aeson.decode () Int)
(declare-fun Network.HTTP.Types.Header.ByteRangeFromTo () Int)
(declare-fun GHC.Types.Module () Int)
(declare-fun GHC.Stack.Types.FreezeCallStack () Int)
(declare-fun Network.HTTP.Types.Method.OPTIONS () Int)
(declare-fun GHC.Types.TrNameD () Int)
(declare-fun GHC.Base.$58$$124$ () Int)
(declare-fun fromJust () Int)
(declare-fun Network.HTTP.Types.Version.$36$WHttpVersion () Int)
(declare-fun GHC.IO.IOMode.ReadMode () Int)
(declare-fun Data.Aeson.Types.Internal.Array () Int)
(declare-fun Network.Wai.Internal.ResponseBuilder () Int)
(declare-fun GHC.Exception.Type.SomeException () Int)
(declare-fun Data.Unique.Really.Unique () Int)
(declare-fun Data.Functor.Const.$36$fFunctorConst () Int)
(declare-fun Data.Semigroup.Internal.Any () Int)
(declare-fun Data.HashMap.Internal.Leaf () Int)
(declare-fun Data.Foldable.length () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$fFromJSONValue () Int)
(declare-fun GHC.IO.IOMode.ReadWriteMode () Int)
(declare-fun Network.Socket.Types.SockAddrUnix () Int)
(declare-fun GHC.Classes.not () Int)
(declare-fun AWSLambda.Events.APIGateway.$36$WAPIGatewayProxyResponse () Int)
(declare-fun GHC.List.iterate () Int)
(declare-fun Data.Text.Internal.Text () Int)
(declare-fun AWSLambda.Events.APIGateway.APIGatewayProxyRequest () Int)
(declare-fun GHC.IO.Exception.ResourceExhausted () Int)
(declare-fun Data.Monoid.Last () Int)
(declare-fun Data.Semigroup.Internal.All () Int)
(declare-fun Data.ByteString.Internal.PS () Int)
(declare-fun GHC.IO.Exception.InappropriateType () Int)
(declare-fun GHC.IO.Exception.HardwareFault () Int)
(declare-fun Data.ByteString.empty () Int)
(declare-fun addrLen () Int)
(declare-fun GHC.Real.$36$W$58$$37$ () Int)
(declare-fun GHC.List.splitAt () Int)
(declare-fun papp3 () Int)
(declare-fun GHC.List.scanl1 () Int)
(declare-fun GHC.Real.toInteger () Int)
(declare-fun Data.Foldable.null () Int)
(declare-fun GHC.IORef.newIORef () Int)
(declare-fun Data.Aeson.Types.Internal.Bool () Int)
(declare-fun Network.Wai.Internal.ChunkedBody () Int)
(declare-fun Data.Typeable.Internal.SomeTypeRep () Int)
(declare-fun GHC.Classes.$38$$38$ () Int)
(declare-fun Network.Wai.Internal.FilePart () Int)
(declare-fun GHC.Classes.$124$$124$ () Int)
(declare-fun GHC.List.dropWhile () Int)
(declare-fun AWSLambda.Events.APIGateway.responseOK () Int)
(declare-fun GHC.IO.Exception.$36$fExceptionIOException () Int)
(declare-fun Protolude.Debug.undefined () Int)
(declare-fun Data.Aeson.Types.Internal.String () Int)
(declare-fun GHC.Enum.C$58$Bounded () Int)
(declare-fun Network.HTTP.Types.Header.ByteRangeSuffix () Int)
(declare-fun Data.Type.Coercion.Coercion () Int)
(declare-fun GHC.List.init () Int)
(declare-fun GHC.IO.Exception.NoSuchThing () Int)
(declare-fun GHC.List.scanr1 () Int)
(declare-fun GHC.Types.True () Bool)
(declare-fun GHC.CString.unpackCString$35$ () Int)
(declare-fun Protolude.Panic.FatalError () Int)
(declare-fun GHC.List.break () Int)
(declare-fun Network.Wai.Internal.KnownLength () Int)
(declare-fun GHC.Base.$62$$62$ () Int)
(declare-fun Data.Maybe.maybe () Int)
(declare-fun Control.Lens.Internal.Setter.$36$fSettableIdentity () Int)
(declare-fun GHC.Stack.Types.EmptyCallStack () Int)
(declare-fun x_Tuple33 () Int)
(declare-fun Data.Either.Right () Int)
(declare-fun GHC.IO.Exception.UserError () Int)
(declare-fun Foreign.C.Types.CChar () Int)
(declare-fun Network.HTTP.Types.Method.PUT () Int)
(declare-fun GHC.IO.MaskedInterruptible () Int)
(declare-fun Network.HTTP.Types.URI.QE () Int)
(declare-fun GHC.IO.Exception.PermissionDenied () Int)
(declare-fun Network.Socket.Types.SockAddrInet () Int)
(declare-fun Network.Wai.Internal.Request () Int)
(declare-fun cast_as () Int)
(declare-fun GHC.Ptr.Ptr () Int)
(declare-fun Data.Text.Internal.$36$WText () Int)
(declare-fun AWSLambda.Events.APIGateway.agprqQueryStringParameters () Int)
(declare-fun GHC.List.head () Int)
(declare-fun Data.ByteString.Lazy.Internal.$36$WChunk () Int)
(declare-fun GHC.Base.$36$fApplicativeIO () Int)
(declare-fun Data.CaseInsensitive.Internal.$36$WCI () Int)
(declare-fun Data.Semigroup.Option () Int)
(declare-fun cast_as_int () Int)
(declare-fun Data.HashMap.Internal.$36$WLeaf () Int)
(declare-fun GHC.Num.$43$ () Int)
(declare-fun GHC.Base.$36$fMonadIO () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WObject () Int)
(declare-fun Control.Lens.Getter.$94$. () Int)
(declare-fun len () Int)
(declare-fun AWSLambda.Events.APIGateway.responseBodyEmbedded () Int)
(declare-fun Data.Type.Equality.Refl () Int)
(declare-fun Data.HashMap.Internal.Full () Int)
(declare-fun GHC.Types.LT () Int)
(declare-fun GHC.Tuple.$40$$44$$44$$41$ () Int)
(declare-fun papp1 () Int)
(declare-fun Data.HashMap.Internal.Empty () Int)
(declare-fun GHC.IO.Exception.EOF () Int)
(declare-fun Data.ByteString.Internal.$36$fMonoidByteString () Int)
(declare-fun GHC.List.drop () Int)
(declare-fun Network.HTTP.Types.Method.POST () Int)
(declare-fun GHC.IO.IOMode.AppendMode () Int)
(declare-fun papp6 () Int)
(declare-fun GHC.Real.mod () Int)
(declare-fun Data.HashMap.Internal.Collision () Int)
(declare-fun GHC.Classes.C$58$IP () Int)
(declare-fun Data.Either.Left () Int)
(declare-fun Data.Functor.Identity.$36$fFunctorIdentity () Int)
(declare-fun Data.Functor.Const.Const () Int)
(declare-fun GHC.List.zip () Int)
(declare-fun liquid_internal_this () Int)
(declare-fun GHC.Real.fromRational () Int)
(declare-fun Control.Lens.Setter.$37$$126$ () Int)
(declare-fun GHC.Classes.$62$ () Int)
(declare-fun Data.Monoid.Ap () Int)
(declare-fun GHC.Tuple.$40$$41$ () Int)
(declare-fun lqdc$35$$35$$36$select$35$$35$GHC.Maybe.Just$35$$35$1 () Int)
(declare-fun GHC.Types.I$35$ () Int)
(declare-fun GHC.IO.Unmasked () Int)
(declare-fun GHC.IO.Exception.Interrupted () Int)
(declare-fun GHC.Generics.K1 () Int)
(declare-fun Data.ByteString.Lazy.Internal.Empty () Int)
(declare-fun x_Tuple31 () Int)
(declare-fun AWSLambda.Events.APIGateway.agprqHeaders () Int)
(declare-fun Data.Text.Internal.Lazy.Empty () Int)
(declare-fun Control.Monad.Trans.Except.ExceptT () Int)
(declare-fun GHC.Num.Integer.IS () Int)
(declare-fun GHC.Word.W16$35$ () Int)
(declare-fun GHC.Real.quot () Int)
(declare-fun Data.ByteString.Builder.toLazyByteString () Int)
(declare-fun Data.Tuple.fst () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WArray () Int)
(declare-fun Data.Aeson.Types.Internal.Null () Int)
(declare-fun GHC.Word.W64$35$ () Int)
(declare-fun Network.HTTP.Types.Method.GET () Int)
(declare-fun GHC.IO.Exception.IllegalOperation () Int)
(declare-fun Data.Tuple.snd () Int)
(declare-fun GHC.IO.Exception.ResourceBusy () Int)
(declare-fun GHC.Types.EQ () Int)
(declare-fun Data.Type.Equality.$36$WRefl () Int)
(declare-fun Control.Monad.IO.Class.$36$fMonadIOIO () Int)
(declare-fun GHC.ExecutionStack.Internal.Location () Int)
(declare-fun GHC.Real.$58$$37$ () Int)
(declare-fun fst () Int)
(declare-fun Data.Text.Encoding.decodeUtf8 () Int)
(declare-fun GHC.List.takeWhile () Int)
(declare-fun AWSLambda.Events.APIGateway.agprqPath () Int)
(declare-fun GHC.Num.negate () Int)
(declare-fun GHC.Num.$45$ () Int)
(declare-fun GHC.Real.rem () Int)
(declare-fun Network.Socket.Types.$36$WSockAddrInet () Int)
(declare-fun GHC.Classes.max () Int)
(declare-fun GHC.List.scanr () Int)
(declare-fun GHC.Real.recip () Int)
(declare-fun Data.Text.$36$fIsStringText () Int)
(declare-fun Network.HTTP.Types.Header.$36$WByteRangeFrom () Int)
(declare-fun autolen () Int)
(declare-fun GHC.IO.Exception.TimeExpired () Int)
(declare-fun GHC.List.replicate () Int)
(declare-fun AWSLambda.Events.APIGateway.APIGatewayProxyResponse () Int)
(declare-fun GHC.Base.. () Int)
(declare-fun GHC.List.take () Int)
(declare-fun GHC.IO.Exception.OtherError () Int)
(declare-fun papp4 () Int)
(declare-fun Network.Socket.Types.$36$WSockAddrInet6 () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WString () Int)
(declare-fun GHC.Stack.Types.PushCallStack () Int)
(declare-fun Network.HTTP.Types.Method.TRACE () Int)
(declare-fun GHC.Real.quotRem () Int)
(declare-fun Network.Socket.Types.SockAddrInet6 () Int)
(declare-fun GHC.Classes.$60$$61$ () Int)
(declare-fun GHC.IO.Exception.AlreadyExists () Int)
(declare-fun GHC.Stack.Types.SrcLoc () Int)
(declare-fun GHC.Types.Eq$35$ () Int)
(declare-fun Network.HTTP.Types.Method.CONNECT () Int)
(declare-fun GHC.Num.fromInteger () Int)
(declare-fun GHC.IO.Exception.ProtocolError () Int)
(declare-fun GHC.Classes.$60$ () Int)
(declare-fun Network.HTTP.Types.URI.QN () Int)
(declare-fun Data.Monoid.First () Int)
(declare-fun Data.Functor.Contravariant.$36$fContravariantConst () Int)
(declare-fun GHC.List.filter () Int)
(declare-fun x_Tuple21 () Int)
(declare-fun Data.HashMap.Internal.$36$WFull () Int)
(declare-fun Data.HashMap.Internal.$36$WBitmapIndexed () Int)
(declare-fun GHC.List.repeat () Int)
(declare-fun Data.Text.Encoding.Error.EncodeError () Int)
(declare-fun head () Int)
(declare-fun GHC.Maybe.Nothing () Int)
(declare-fun GHC.Generics.Comp1 () Int)
(declare-fun Data.Aeson.Embedded.Embedded () Int)
(declare-fun Network.HTTP.Types.Header.ByteRangeFrom () Int)
(declare-fun Data.CaseInsensitive.Internal.CI () Int)
(declare-fun GHC.TypeLits.SomeSymbol () Int)
(declare-fun Control.Concurrent.QSemN.$36$WQSemN () Int)
(declare-fun Data.Text.splitOn () Int)
(declare-fun Network.HTTP.Types.Method.PATCH () Int)
(declare-fun GHC.Num.Integer.IP () Int)
(declare-fun AWSLambda.Events.APIGateway.requestBodyEmbedded () Int)
(declare-fun GHC.Base.map () Int)
(declare-fun GHC.IO.Exception.InvalidArgument () Int)
(declare-fun GHC.Base.flip () Int)
(declare-fun x_Tuple32 () Int)
(declare-fun Data.Vault.ST.Lazy.Vault () Int)
(declare-fun Data.Aeson.Types.Internal.Number () Int)
(declare-fun Network.HTTP.Types.Header.$36$WByteRangeSuffix () Int)
(declare-fun GHC.IO.Exception.SystemError () Int)
(declare-fun GHC.Err.error () Int)
(declare-fun Data.HashMap.Internal.BitmapIndexed () Int)
(declare-fun GHC.Classes.compare () Int)
(declare-fun Network.Wai.Internal.ResponseFile () Int)
(declare-fun GHC.Types.TrNameS () Int)
(declare-fun Data.Aeson.encode () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fToJSONValue () Int)
(declare-fun GHC.Num.Integer.IN () Int)
(declare-fun Data.Functor.Identity.Identity () Int)
(declare-fun GHC.MVar.MVar () Int)
(declare-fun Data.ByteString.Internal.$36$fIsStringByteString () Int)
(declare-fun Network.Wai.Internal.ResponseReceived () Int)
(declare-fun GHC.Num.$42$ () Int)
(declare-fun GHC.Types.False () Bool)
(declare-fun GHC.Weak.Weak () Int)
(declare-fun GHC.Generics.M1 () Int)
(declare-fun Data.Typeable.Internal.C$58$Typeable () Int)
(declare-fun Control.Applicative.ZipList () Int)
(declare-fun Data.CaseInsensitive.Internal.$36$fFoldCaseByteString0 () Int)
(declare-fun Protolude.throwIO () Int)
(declare-fun AWSLambda.Events.APIGateway.agprqHttpMethod () Int)
(declare-fun papp2 () Int)
(declare-fun GHC.Classes.$47$$61$ () Int)
(declare-fun GHC.List.cycle () Int)
(declare-fun Data.String.fromString () Int)
(declare-fun GHC.IO.Handle.Types.$36$WDuplexHandle () Int)
(declare-fun GHC.Types.GT () Int)
(declare-fun GHC.TypeNats.C$58$KnownNat () Int)
(declare-fun AWSLambda.Events.APIGateway.agprsHeaders () Int)
(declare-fun apply$35$$35$13 (Int (_ BitVec 32)) Bool)
(declare-fun apply$35$$35$1 (Int Int) Bool)
(declare-fun apply$35$$35$3 (Int Int) (_ BitVec 32))
(declare-fun apply$35$$35$15 (Int (_ BitVec 32)) (_ BitVec 32))
(declare-fun apply$35$$35$14 (Int (_ BitVec 32)) Str)
(declare-fun apply$35$$35$9 (Int Str) Bool)
(declare-fun apply$35$$35$4 (Int Bool) Int)
(declare-fun apply$35$$35$10 (Int Str) Str)
(declare-fun apply$35$$35$11 (Int Str) (_ BitVec 32))
(declare-fun apply$35$$35$12 (Int (_ BitVec 32)) Int)
(declare-fun apply$35$$35$8 (Int Str) Int)
(declare-fun apply$35$$35$0 (Int Int) Int)
(declare-fun apply$35$$35$6 (Int Bool) Str)
(declare-fun apply$35$$35$7 (Int Bool) (_ BitVec 32))
(declare-fun apply$35$$35$2 (Int Int) Str)
(declare-fun apply$35$$35$5 (Int Bool) Bool)
(declare-fun coerce$35$$35$13 ((_ BitVec 32)) Bool)
(declare-fun coerce$35$$35$1 (Int) Bool)
(declare-fun coerce$35$$35$3 (Int) (_ BitVec 32))
(declare-fun coerce$35$$35$15 ((_ BitVec 32)) (_ BitVec 32))
(declare-fun coerce$35$$35$14 ((_ BitVec 32)) Str)
(declare-fun coerce$35$$35$9 (Str) Bool)
(declare-fun coerce$35$$35$4 (Bool) Int)
(declare-fun coerce$35$$35$10 (Str) Str)
(declare-fun coerce$35$$35$11 (Str) (_ BitVec 32))
(declare-fun coerce$35$$35$12 ((_ BitVec 32)) Int)
(declare-fun coerce$35$$35$8 (Str) Int)
(declare-fun coerce$35$$35$0 (Int) Int)
(declare-fun coerce$35$$35$6 (Bool) Str)
(declare-fun coerce$35$$35$7 (Bool) (_ BitVec 32))
(declare-fun coerce$35$$35$2 (Int) Str)
(declare-fun coerce$35$$35$5 (Bool) Bool)
(declare-fun smt_lambda$35$$35$13 ((_ BitVec 32) Bool) Int)
(declare-fun smt_lambda$35$$35$1 (Int Bool) Int)
(declare-fun smt_lambda$35$$35$3 (Int (_ BitVec 32)) Int)
(declare-fun smt_lambda$35$$35$15 ((_ BitVec 32) (_ BitVec 32)) Int)
(declare-fun smt_lambda$35$$35$14 ((_ BitVec 32) Str) Int)
(declare-fun smt_lambda$35$$35$9 (Str Bool) Int)
(declare-fun smt_lambda$35$$35$4 (Bool Int) Int)
(declare-fun smt_lambda$35$$35$10 (Str Str) Int)
(declare-fun smt_lambda$35$$35$11 (Str (_ BitVec 32)) Int)
(declare-fun smt_lambda$35$$35$12 ((_ BitVec 32) Int) Int)
(declare-fun smt_lambda$35$$35$8 (Str Int) Int)
(declare-fun smt_lambda$35$$35$0 (Int Int) Int)
(declare-fun smt_lambda$35$$35$6 (Bool Str) Int)
(declare-fun smt_lambda$35$$35$7 (Bool (_ BitVec 32)) Int)
(declare-fun smt_lambda$35$$35$2 (Int Str) Int)
(declare-fun smt_lambda$35$$35$5 (Bool Bool) Int)
(declare-fun lam_arg$35$$35$1$35$$35$4 () Bool)
(declare-fun lam_arg$35$$35$2$35$$35$4 () Bool)
(declare-fun lam_arg$35$$35$3$35$$35$4 () Bool)
(declare-fun lam_arg$35$$35$4$35$$35$4 () Bool)
(declare-fun lam_arg$35$$35$5$35$$35$4 () Bool)
(declare-fun lam_arg$35$$35$6$35$$35$4 () Bool)
(declare-fun lam_arg$35$$35$7$35$$35$4 () Bool)
(declare-fun lam_arg$35$$35$1$35$$35$12 () (_ BitVec 32))
(declare-fun lam_arg$35$$35$2$35$$35$12 () (_ BitVec 32))
(declare-fun lam_arg$35$$35$3$35$$35$12 () (_ BitVec 32))
(declare-fun lam_arg$35$$35$4$35$$35$12 () (_ BitVec 32))
(declare-fun lam_arg$35$$35$5$35$$35$12 () (_ BitVec 32))
(declare-fun lam_arg$35$$35$6$35$$35$12 () (_ BitVec 32))
(declare-fun lam_arg$35$$35$7$35$$35$12 () (_ BitVec 32))
(declare-fun lam_arg$35$$35$1$35$$35$8 () Str)
(declare-fun lam_arg$35$$35$2$35$$35$8 () Str)
(declare-fun lam_arg$35$$35$3$35$$35$8 () Str)
(declare-fun lam_arg$35$$35$4$35$$35$8 () Str)
(declare-fun lam_arg$35$$35$5$35$$35$8 () Str)
(declare-fun lam_arg$35$$35$6$35$$35$8 () Str)
(declare-fun lam_arg$35$$35$7$35$$35$8 () Str)
(declare-fun lam_arg$35$$35$1$35$$35$0 () Int)
(declare-fun lam_arg$35$$35$2$35$$35$0 () Int)
(declare-fun lam_arg$35$$35$3$35$$35$0 () Int)
(declare-fun lam_arg$35$$35$4$35$$35$0 () Int)
(declare-fun lam_arg$35$$35$5$35$$35$0 () Int)
(declare-fun lam_arg$35$$35$6$35$$35$0 () Int)
(declare-fun lam_arg$35$$35$7$35$$35$0 () Int)




(assert (distinct GHC.IO.Exception.SystemError GHC.IO.Exception.InvalidArgument GHC.IO.Exception.ProtocolError GHC.IO.Exception.AlreadyExists GHC.IO.Exception.OtherError GHC.IO.Exception.TimeExpired GHC.IO.Exception.ResourceBusy GHC.IO.Exception.IllegalOperation GHC.IO.Exception.Interrupted GHC.IO.Exception.EOF GHC.IO.Exception.PermissionDenied GHC.IO.Exception.UserError GHC.IO.Exception.NoSuchThing GHC.IO.Exception.HardwareFault GHC.IO.Exception.InappropriateType GHC.IO.Exception.ResourceExhausted GHC.IO.Exception.UnsupportedOperation GHC.IO.Exception.UnsatisfiedConstraints GHC.IO.Exception.ResourceVanished))
(assert (distinct Network.HTTP.Types.Method.PATCH Network.HTTP.Types.Method.CONNECT Network.HTTP.Types.Method.TRACE Network.HTTP.Types.Method.GET Network.HTTP.Types.Method.POST Network.HTTP.Types.Method.PUT Network.HTTP.Types.Method.OPTIONS Network.HTTP.Types.Method.HEAD Network.HTTP.Types.Method.DELETE))

(assert (distinct GHC.Types.GT GHC.Types.EQ GHC.Types.LT))







(assert (distinct GHC.Types.False GHC.Types.True))
(assert (distinct GHC.IO.Unmasked GHC.IO.MaskedInterruptible GHC.IO.MaskedUninterruptible))



(assert (distinct GHC.IO.IOMode.AppendMode GHC.IO.IOMode.ReadWriteMode GHC.IO.IOMode.ReadMode GHC.IO.IOMode.WriteMode))




(push 1)
(push 1)
(pop 1)
(pop 1)
(exit)
