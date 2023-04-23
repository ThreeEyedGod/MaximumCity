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
(declare-fun ExternalInterfaces.ServantShim.makeHandler () Int)
(declare-fun InterfaceAdapters.Config.CL () Int)
(declare-fun GHC.Types.$91$$93$ () Int)
(declare-fun InterfaceAdapters.Config.PirateWeather () Int)
(declare-fun GHC.Base.$36$fSemigroup$91$$93$ () Int)
(declare-fun GHC.Real.divMod () Int)
(declare-fun GHC.List.reverse () Int)
(declare-fun GHC.List.scanl () Int)
(declare-fun GHC.Classes.min () Int)
(declare-fun GHC.Types.C$35$ () Int)
(declare-fun GHC.Real.$47$ () Int)
(declare-fun GHC.Types.$58$ () Int)
(declare-fun tail () Int)
(declare-fun isJust () Int)
(declare-fun InterfaceAdapters.Config.Whatsapp () Int)
(declare-fun GHC.Real.div () Int)
(declare-fun AWSLambda.Events.APIGateway.$36$WAPIGatewayProxyRequest () Int)
(declare-fun GHC.Exception.Type.$36$p2Exception () Int)
(declare-fun GHC.Classes.$62$$61$ () Int)
(declare-fun GHC.List.span () Int)
(declare-fun GHC.Base.$60$$62$ () Int)
(declare-fun GHC.Tuple.$40$$44$$41$ () Int)
(declare-fun GHC.Real.$94$ () Int)
(declare-fun GHC.Classes.$61$$61$ () Int)
(declare-fun GHC.List.$33$$33$ () Int)
(declare-fun GHC.Base.$36$ () Int)
(declare-fun GHC.Show.show () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WBool () Int)
(declare-fun GHC.List.last () Int)
(declare-fun Data.Aeson.Types.Internal.Object () Int)
(declare-fun GHC.List.zipWith () Int)
(declare-fun papp5 () Int)
(declare-fun snd () Int)
(declare-fun GHC.Base.$43$$43$ () Int)
(declare-fun GHC.Maybe.Just () Int)
(declare-fun x_Tuple22 () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WNumber () Int)
(declare-fun InterfaceAdapters.Config.Signal () Int)
(declare-fun GHC.Base.$62$$62$$61$ () Int)
(declare-fun GHC.Real.fromIntegral () Int)
(declare-fun GHC.IO.Exception.IOError () Int)
(declare-fun GHC.List.tail () Int)
(declare-fun GHC.Types.Module () Int)
(declare-fun GHC.Stack.Types.FreezeCallStack () Int)
(declare-fun GHC.Types.TrNameD () Int)
(declare-fun fromJust () Int)
(declare-fun Data.Aeson.Types.Internal.Array () Int)
(declare-fun GHC.Exception.Type.SomeException () Int)
(declare-fun Data.Foldable.length () Int)
(declare-fun Data.Aeson.Types.FromJSON.$36$fFromJSONValue () Int)
(declare-fun GHC.Classes.not () Int)
(declare-fun AWSLambda.Events.APIGateway.$36$WAPIGatewayProxyResponse () Int)
(declare-fun GHC.List.iterate () Int)
(declare-fun AWSLambda.Events.APIGateway.APIGatewayProxyRequest () Int)
(declare-fun addrLen () Int)
(declare-fun GHC.Real.$36$W$58$$37$ () Int)
(declare-fun GHC.List.splitAt () Int)
(declare-fun papp3 () Int)
(declare-fun GHC.List.scanl1 () Int)
(declare-fun GHC.Real.toInteger () Int)
(declare-fun Data.Foldable.null () Int)
(declare-fun Data.Aeson.Types.Internal.Bool () Int)
(declare-fun GHC.Classes.$38$$38$ () Int)
(declare-fun GHC.Classes.$124$$124$ () Int)
(declare-fun GHC.List.dropWhile () Int)
(declare-fun GHC.Exception.Type.$36$fExceptionSomeException () Int)
(declare-fun ExternalInterfaces.ApplicationAssembly.servApp () Int)
(declare-fun Data.Aeson.Types.Internal.String () Int)
(declare-fun GHC.Enum.C$58$Bounded () Int)
(declare-fun GHC.List.init () Int)
(declare-fun GHC.List.scanr1 () Int)
(declare-fun GHC.Types.True () Bool)
(declare-fun GHC.CString.unpackCString$35$ () Int)
(declare-fun InterfaceAdapters.Config.AWSAPIRest () Int)
(declare-fun GHC.List.break () Int)
(declare-fun Data.Aeson.Embedded.$36$fFromTextEmbedded () Int)
(declare-fun InterfaceAdapters.Config.Web () Int)
(declare-fun GHC.Stack.Types.EmptyCallStack () Int)
(declare-fun x_Tuple33 () Int)
(declare-fun Data.Either.Right () Int)
(declare-fun InterfaceAdapters.Config.OpenCage () Int)
(declare-fun Data.Aeson.Embedded.$36$fToTextEmbedded () Int)
(declare-fun cast_as () Int)
(declare-fun GHC.List.head () Int)
(declare-fun cast_as_int () Int)
(declare-fun InterfaceAdapters.Config.Telegram () Int)
(declare-fun GHC.Num.$43$ () Int)
(declare-fun GHC.Base.$36$fMonadIO () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WObject () Int)
(declare-fun len () Int)
(declare-fun InterfaceAdapters.Config.OpenWeather () Int)
(declare-fun GHC.Types.LT () Int)
(declare-fun GHC.Tuple.$40$$44$$44$$41$ () Int)
(declare-fun papp1 () Int)
(declare-fun GHC.TopHandler.runMainIO () Int)
(declare-fun GHC.List.drop () Int)
(declare-fun papp6 () Int)
(declare-fun GHC.Real.mod () Int)
(declare-fun GHC.Classes.C$58$IP () Int)
(declare-fun Data.Either.Left () Int)
(declare-fun InterfaceAdapters.Config.Config () Int)
(declare-fun GHC.List.zip () Int)
(declare-fun liquid_internal_this () Int)
(declare-fun GHC.Real.fromRational () Int)
(declare-fun GHC.Classes.$62$ () Int)
(declare-fun GHC.Tuple.$40$$41$ () Int)
(declare-fun lqdc$35$$35$$36$select$35$$35$GHC.Maybe.Just$35$$35$1 () Int)
(declare-fun GHC.Types.I$35$ () Int)
(declare-fun InterfaceAdapters.Config.PositionStack () Int)
(declare-fun x_Tuple31 () Int)
(declare-fun GHC.Num.Integer.IS () Int)
(declare-fun GHC.Real.quot () Int)
(declare-fun Control.Exception.Base.handle () Int)
(declare-fun Data.Tuple.fst () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WArray () Int)
(declare-fun Data.Aeson.Types.Internal.Null () Int)
(declare-fun Data.Tuple.snd () Int)
(declare-fun GHC.Types.EQ () Int)
(declare-fun GHC.Real.$58$$37$ () Int)
(declare-fun fst () Int)
(declare-fun GHC.List.takeWhile () Int)
(declare-fun GHC.Num.negate () Int)
(declare-fun GHC.Num.$45$ () Int)
(declare-fun GHC.Real.rem () Int)
(declare-fun GHC.Classes.max () Int)
(declare-fun GHC.List.scanr () Int)
(declare-fun GHC.Real.recip () Int)
(declare-fun autolen () Int)
(declare-fun GHC.List.replicate () Int)
(declare-fun AWSLambda.Events.APIGateway.APIGatewayProxyResponse () Int)
(declare-fun GHC.Base.. () Int)
(declare-fun GHC.List.take () Int)
(declare-fun papp4 () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WString () Int)
(declare-fun GHC.Stack.Types.PushCallStack () Int)
(declare-fun GHC.Real.quotRem () Int)
(declare-fun GHC.Classes.$60$$61$ () Int)
(declare-fun GHC.Num.fromInteger () Int)
(declare-fun GHC.Classes.$60$ () Int)
(declare-fun GHC.List.filter () Int)
(declare-fun x_Tuple21 () Int)
(declare-fun GHC.List.repeat () Int)
(declare-fun head () Int)
(declare-fun GHC.Maybe.Nothing () Int)
(declare-fun Data.Aeson.Embedded.Embedded () Int)
(declare-fun GHC.Num.Integer.IP () Int)
(declare-fun GHC.Base.map () Int)
(declare-fun InterfaceAdapters.Config.Other () Int)
(declare-fun x_Tuple32 () Int)
(declare-fun Data.Aeson.Types.Internal.Number () Int)
(declare-fun GHC.Err.error () Int)
(declare-fun GHC.Classes.compare () Int)
(declare-fun Network.Wai.Internal.ResponseFile () Int)
(declare-fun AWSLambda.Events.APIGateway.apiGatewayMain () Int)
(declare-fun GHC.Types.TrNameS () Int)
(declare-fun Data.Aeson.Types.ToJSON.$36$fToJSONValue () Int)
(declare-fun GHC.Num.Integer.IN () Int)
(declare-fun System.IO.putStrLn () Int)
(declare-fun Network.Wai.Internal.ResponseReceived () Int)
(declare-fun GHC.Num.$42$ () Int)
(declare-fun GHC.Types.False () Bool)
(declare-fun papp2 () Int)
(declare-fun GHC.Classes.$47$$61$ () Int)
(declare-fun GHC.List.cycle () Int)
(declare-fun GHC.Types.GT () Int)
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

(assert (distinct InterfaceAdapters.Config.OpenWeather InterfaceAdapters.Config.PirateWeather))

(assert (distinct InterfaceAdapters.Config.PositionStack InterfaceAdapters.Config.OpenCage))

(assert (distinct InterfaceAdapters.Config.Other InterfaceAdapters.Config.AWSAPIRest))
(assert (distinct GHC.Types.GT GHC.Types.EQ GHC.Types.LT))

(assert (distinct InterfaceAdapters.Config.Telegram InterfaceAdapters.Config.Web InterfaceAdapters.Config.Signal InterfaceAdapters.Config.Whatsapp InterfaceAdapters.Config.CL))
(assert (distinct GHC.Types.False GHC.Types.True))




(push 1)
(push 1)
(pop 1)
(pop 1)
(exit)
