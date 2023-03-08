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
(declare-fun lq_tmp$36$x$35$$35$2196 () Int)
(declare-fun GHC.Types.KindRepTypeLitS () Int)
(declare-fun InterfaceAdapters.Telegram.Telegram.getMeta () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999863$35$$35$d53NR () Int)
(declare-fun uuid$35$$35$a53Gq () Int)
(declare-fun GHC.Types.$91$$93$ () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999867$35$$35$d53NV () Int)
(declare-fun fix$36$$36$krep_a53KD () Int)
(declare-fun GHC.Types.Word32Rep () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999877$35$$35$d53O5 () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999871$35$$35$d53NZ () Int)
(declare-fun resp$35$$35$a53Gs () Int)
(declare-fun InterfaceAdapters.Utils.Helper.SomeIOError () Int)
(declare-fun lq_tmp$36$x$35$$35$2335 () Int)
(declare-fun lq_tmp$36$x$35$$35$1987 () Int)
(declare-fun InterfaceAdapters.Preferences.getPreferences () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999896$35$$35$d53Oo () Int)
(declare-fun InterfaceAdapters.Telegram.Telegram.gettheTelegram () Int)
(declare-fun GHC.Types.C$35$ () Int)
(declare-fun lq_tmp$36$x$35$$35$2624 () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999892$35$$35$d53Ok () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999880$35$$35$d53O8 () Int)
(declare-fun Polysemy.Internal.$36$fMonadSem () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999860$35$$35$d53NO () Int)
(declare-fun lq_tmp$36$x$35$$35$5070 () Int)
(declare-fun lq_tmp$36$x$35$$35$5069 () Int)
(declare-fun lq_tmp$36$x$35$$35$4644 () Int)
(declare-fun GHC.Types.$58$ () Int)
(declare-fun tail () Int)
(declare-fun updt$35$$35$a53Gp () Int)
(declare-fun Data.Aeson.Types.ToJSON.C$58$ToJSON () Int)
(declare-fun lq_tmp$36$x$35$$35$1628 () Int)
(declare-fun Polysemy.Embed.Type.$36$WEmbed () Int)
(declare-fun lq_tmp$36$x$35$$35$4643 () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999870$35$$35$d53NY () Int)
(declare-fun lq_tmp$36$x$35$$35$1631 () Int)
(declare-fun GHC.Types.Word8Rep () Int)
(declare-fun InterfaceAdapters.Utils.Helper.MissingEnvError () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999904$35$$35$d53Ow () Int)
(declare-fun fix$36$$36$cgetInfo$35$$35$a53Hy () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999899$35$$35$d53Or () Int)
(declare-fun tlgm$35$$35$a53Gr () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999909$35$$35$d53OB () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999900$35$$35$d53Os () Int)
(declare-fun UseCases.AgricultureUseCase.C$58$UserInput () Int)
(declare-fun UseCases.WWI.$36$WSendBackMsg () Int)
(declare-fun lq_tmp$36$x$35$$35$1651 () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999885$35$$35$d53Od () Int)
(declare-fun ds_d53Nn () Int)
(declare-fun lq_tmp$36$x$35$$35$4667 () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999874$35$$35$d53O2 () Int)
(declare-fun GHC.Tuple.$40$$44$$41$ () Int)
(declare-fun lq_tmp$36$x$35$$35$2562 () Int)
(declare-fun GHC.Classes.$61$$61$ () Int)
(declare-fun InterfaceAdapters.Preferences.setPreferences () Int)
(declare-fun fix$36$$36$cgetInfo$35$$35$a53HW () Int)
(declare-fun lq_tmp$36$x$35$$35$4039 () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WBool () Int)
(declare-fun Data.Aeson.Types.Internal.Object () Int)
(declare-fun papp5 () Int)
(declare-fun fix$36$$36$d$40$$37$$44$$37$$41$_a53I0 () Int)
(declare-fun lq_tmp$36$x$35$$35$1654 () Int)
(declare-fun GHC.Types.Word16Rep () Int)
(declare-fun lq_tmp$36$x$35$$35$3367 () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999914$35$$35$d53OG () Int)
(declare-fun GHC.Maybe.Just () Int)
(declare-fun x_Tuple22 () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999886$35$$35$d53Oe () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WNumber () Int)
(declare-fun lq_tmp$36$x$35$$35$1910 () Int)
(declare-fun GHC.Generics.C$58$Generic () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999882$35$$35$d53Oa () Int)
(declare-fun GHC.Base.pure () Int)
(declare-fun lq_tmp$36$x$35$$35$1909 () Int)
(declare-fun GHC.Base.$62$$62$$61$ () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999918$35$$35$d53OK () Int)
(declare-fun GHC.IO.Exception.IOError () Int)
(declare-fun fix$36$$36$d$40$$37$$44$$37$$41$_a53HC () Int)
(declare-fun fix$36$$36$d$40$$37$$44$$37$$41$_a53K8 () Int)
(declare-fun lq_tmp$36$x$35$$35$5661 () Int)
(declare-fun GHC.Types.Module () Int)
(declare-fun GHC.Stack.Types.FreezeCallStack () Int)
(declare-fun GHC.Types.TrNameD () Int)
(declare-fun lq_tmp$36$x$35$$35$4923 () Int)
(declare-fun GHC.Types.Word64Rep () Int)
(declare-fun GHC.Types.TyCon () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999911$35$$35$d53OD () Int)
(declare-fun GHC.Types.UnliftedRep () Int)
(declare-fun lq_tmp$36$x$35$$35$1907 () Int)
(declare-fun Data.Aeson.Types.Internal.Array () Int)
(declare-fun GHC.Classes.C$58$$40$$37$$37$$41$ () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999872$35$$35$d53O0 () Int)
(declare-fun lq_tmp$36$x$35$$35$1732 () Int)
(declare-fun lq_tmp$36$x$35$$35$4982 () Int)
(declare-fun lq_tmp$36$x$35$$35$2329 () Int)
(declare-fun GHC.Types.IntRep () Int)
(declare-fun lq_tmp$36$x$35$$35$2055 () Int)
(declare-fun lq_tmp$36$x$35$$35$5000 () Int)
(declare-fun lit$36$UseCases.AgricultureUseCase () Str)
(declare-fun GHC.Types.DoubleRep () Int)
(declare-fun AWSLambda.Events.APIGateway.$36$WAPIGatewayProxyResponse () Int)
(declare-fun Data.Text.Internal.Text () Int)
(declare-fun Polysemy.Internal.$36$fApplicativeSem () Int)
(declare-fun Data.Text.$36$fEqText () Int)
(declare-fun GHC.Types.Int8Rep () Int)
(declare-fun lq_tmp$36$x$35$$35$4747 () Int)
(declare-fun GHC.Real.$36$W$58$$37$ () Int)
(declare-fun lq_tmp$36$x$35$$35$5805 () Int)
(declare-fun GHC.Types.KindRepFun () Int)
(declare-fun UseCases.WWI.UserAsk () Int)
(declare-fun GHC.Types.WordRep () Int)
(declare-fun papp3 () Int)
(declare-fun Polysemy.Embed.Type.Embed () Int)
(declare-fun lq_tmp$36$x$35$$35$1652 () Int)
(declare-fun lq_tmp$36$x$35$$35$2192 () Int)
(declare-fun Polysemy.Error.Throw () Int)
(declare-fun Data.Aeson.Types.Internal.Bool () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999878$35$$35$d53O6 () Int)
(declare-fun fix$36$$36$d$40$$37$$44$$37$$41$_a53HD () Int)
(declare-fun Polysemy.Error.throw () Int)
(declare-fun lq_tmp$36$x$35$$35$4924 () Int)
(declare-fun Data.Aeson.Types.Internal.String () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999905$35$$35$d53Ox () Int)
(declare-fun GHC.Enum.C$58$Bounded () Int)
(declare-fun GHC.Types.True () Int)
(declare-fun lq_tmp$36$x$35$$35$1986 () Int)
(declare-fun GHC.CString.unpackCString$35$ () Int)
(declare-fun responseBody$35$$35$a53Gu () Int)
(declare-fun lq_tmp$36$x$35$$35$4921 () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999862$35$$35$d53NQ () Int)
(declare-fun GHC.Base.$62$$62$ () Int)
(declare-fun GHC.Types.$36$WKindRepVar () Int)
(declare-fun GHC.Stack.Types.EmptyCallStack () Int)
(declare-fun x_Tuple33 () Int)
(declare-fun Data.Either.Right () Int)
(declare-fun Data.Aeson.Types.FromJSON.C$58$FromJSON () Int)
(declare-fun GHC.Show.C$58$Show () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999859$35$$35$d53NN () Int)
(declare-fun lq_tmp$36$x$35$$35$1734 () Int)
(declare-fun lq_tmp$36$x$35$$35$4984 () Int)
(declare-fun lq_tmp$36$x$35$$35$1629 () Int)
(declare-fun cast_as () Int)
(declare-fun lq_tmp$36$x$35$$35$4642 () Int)
(declare-fun Data.Text.Internal.$36$WText () Int)
(declare-fun lq_tmp$36$x$35$$35$2817 () Int)
(declare-fun GHC.Types.KindRepApp () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999897$35$$35$d53Op () Int)
(declare-fun lq_tmp$36$x$35$$35$1630 () Int)
(declare-fun Polysemy.Trace.$36$WTrace () Int)
(declare-fun UseCases.AgricultureUseCase.$36$fUserInputUpdate () Int)
(declare-fun cast_as_int () Int)
(declare-fun GHC.Types.F$35$ () Int)
(declare-fun lq_tmp$36$x$35$$35$5659 () Int)
(declare-fun lq_tmp$36$x$35$$35$4748 () Int)
(declare-fun plName$35$$35$a53Gn () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WObject () Int)
(declare-fun lq_tmp$36$x$35$$35$4535 () Int)
(declare-fun lq_tmp$36$x$35$$35$1969 () Int)
(declare-fun lq_tmp$36$x$35$$35$1970 () Int)
(declare-fun lq_tmp$36$x$35$$35$5002 () Int)
(declare-fun lq_tmp$36$x$35$$35$4232 () Int)
(declare-fun UseCases.WWI.SendBackMsg () Int)
(declare-fun GHC.Types.Int64Rep () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999894$35$$35$d53Om () Int)
(declare-fun GHC.Types.LT () Int)
(declare-fun GHC.Tuple.$40$$44$$44$$41$ () Int)
(declare-fun papp1 () Int)
(declare-fun GHC.Classes.$36$p2$40$$37$$44$$37$$41$ () Int)
(declare-fun UseCases.WWI.sendBackMsg () Int)
(declare-fun lq_tmp$36$x$35$$35$4666 () Int)
(declare-fun Polysemy.Trace.Trace () Int)
(declare-fun GHC.Types.AddrRep () Int)
(declare-fun uuid$35$$35$X1 () Int)
(declare-fun fix$36$$36$d$40$$37$$44$$37$$41$_a53KA () Int)
(declare-fun papp6 () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999888$35$$35$d53Og () Int)
(declare-fun GHC.Classes.C$58$IP () Int)
(declare-fun Data.Either.Left () Int)
(declare-fun Polysemy.Trace.trace () Int)
(declare-fun lit$36$UserInput () Str)
(declare-fun GHC.Classes.C$58$Eq () Int)
(declare-fun Polysemy.Internal.embed () Int)
(declare-fun Polysemy.Error.catch () Int)
(declare-fun liquid_internal_this () Int)
(declare-fun lq_tmp$36$x$35$$35$1908 () Int)
(declare-fun GHC.Types.KindRepTyConApp () Int)
(declare-fun GHC.Tuple.$40$$41$ () Int)
(declare-fun GHC.Types.KindRepVar () Int)
(declare-fun GHC.Types.I$35$ () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999891$35$$35$d53Oj () Int)
(declare-fun GHC.Types.KindRepTypeLitD () Int)
(declare-fun x_Tuple31 () Int)
(declare-fun lq_tmp$36$x$35$$35$5068 () Int)
(declare-fun lq_tmp$36$x$35$$35$4645 () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999856$35$$35$d53NK () Int)
(declare-fun GHC.Num.Integer.IS () Int)
(declare-fun GHC.Types.FloatRep () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999883$35$$35$d53Ob () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999889$35$$35$d53Oh () Int)
(declare-fun Data.Tuple.fst () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WArray () Int)
(declare-fun Data.Aeson.Types.Internal.Null () Int)
(declare-fun Web.Telegram.API.Bot.Data.Update () Int)
(declare-fun lq_tmp$36$x$35$$35$3511 () Int)
(declare-fun lq_tmp$36$x$35$$35$1733 () Int)
(declare-fun lq_tmp$36$x$35$$35$4983 () Int)
(declare-fun GHC.Types.LiftedRep () Int)
(declare-fun Data.Text.isInfixOf () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999901$35$$35$d53Ot () Int)
(declare-fun GHC.Types.EQ () Int)
(declare-fun GHC.Real.$58$$37$ () Int)
(declare-fun lq_tmp$36$x$35$$35$2056 () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999855$35$$35$d53NJ () Int)
(declare-fun GHC.Types.VecRep () Int)
(declare-fun InterfaceAdapters.Utils.Helper.EmptyKeyError () Int)
(declare-fun lq_tmp$36$x$35$$35$4533 () Int)
(declare-fun UseCases.WWI.getWeatherTown () Int)
(declare-fun lit$36$MaximumCity$45$0.9.9$45$IUoVqdUkwWmEGL6yhjWbHS () Str)
(declare-fun UseCases.WWI.GetWeatherTown () Int)
(declare-fun GHC.Types.krep$36$$42$ () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999893$35$$35$d53Ol () Int)
(declare-fun lq_tmp$36$x$35$$35$2961 () Int)
(declare-fun lq_tmp$36$x$35$$35$5001 () Int)
(declare-fun Data.Text.$36$fIsStringText () Int)
(declare-fun lit$36$$123$ () Str)
(declare-fun lq_tmp$36$x$35$$35$4668 () Int)
(declare-fun AWSLambda.Events.APIGateway.APIGatewayProxyResponse () Int)
(declare-fun resp$35$$35$X2 () Int)
(declare-fun InterfaceAdapters.Preferences.modalUser () Int)
(declare-fun InterfaceAdapters.Preferences.Preferences () Int)
(declare-fun fix$36$$36$d$40$$37$$44$$37$$41$_a53I1 () Int)
(declare-fun GHC.Types.Int16Rep () Int)
(declare-fun papp4 () Int)
(declare-fun GHC.Types.$36$WKindRepTYPE () Int)
(declare-fun lq_tmp$36$x$35$$35$2195 () Int)
(declare-fun GHC.Types.eq_sel () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999913$35$$35$d53OF () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WString () Int)
(declare-fun GHC.Stack.Types.PushCallStack () Int)
(declare-fun GHC.Types.Eq$35$ () Int)
(declare-fun GHC.Types.KindRepTYPE () Int)
(declare-fun lq_tmp$36$x$35$$35$5791 () Int)
(declare-fun GHC.Types.SumRep () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999881$35$$35$d53O9 () Int)
(declare-fun UseCases.AgricultureUseCase.$36$fUserInputText () Int)
(declare-fun x_Tuple21 () Int)
(declare-fun lq_tmp$36$x$35$$35$2330 () Int)
(declare-fun head () Int)
(declare-fun GHC.Types.$36$tcConstraint () Int)
(declare-fun lq_tmp$36$x$35$$35$4922 () Int)
(declare-fun GHC.Maybe.Nothing () Int)
(declare-fun lq_tmp$36$x$35$$35$5660 () Int)
(declare-fun UseCases.WWI.$36$WGetWeatherTown () Int)
(declare-fun GHC.Classes.C$58$$40$$37$$44$$37$$41$ () Int)
(declare-fun lq_tmp$36$x$35$$35$1735 () Int)
(declare-fun GHC.Num.Integer.IP () Int)
(declare-fun lq_tmp$36$x$35$$35$1988 () Int)
(declare-fun x_Tuple32 () Int)
(declare-fun Data.Aeson.Types.Internal.Number () Int)
(declare-fun GHC.Types.TupleRep () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999875$35$$35$d53O3 () Int)
(declare-fun GHC.Types.TrNameS () Int)
(declare-fun lq_tmp$36$x$35$$35$2054 () Int)
(declare-fun GHC.Num.Integer.IN () Int)
(declare-fun lq_tmp$36$x$35$$35$4534 () Int)
(declare-fun lq_tmp$36$x$35$$35$4376 () Int)
(declare-fun lq_tmp$36$x$35$$35$1968 () Int)
(declare-fun lq_anf$36$$35$$35$7205759403793999907$35$$35$d53Oz () Int)
(declare-fun GHC.Types.False () Int)
(declare-fun lq_tmp$36$x$35$$35$4749 () Int)
(declare-fun Data.Typeable.Internal.C$58$Typeable () Int)
(declare-fun papp2 () Int)
(declare-fun lq_tmp$36$x$35$$35$1653 () Int)
(declare-fun Polysemy.Internal.Union.C$58$Find () Int)
(declare-fun lq_tmp$36$x$35$$35$4665 () Int)
(declare-fun Data.String.fromString () Int)
(declare-fun lq_tmp$36$x$35$$35$4746 () Int)
(declare-fun GHC.Types.GT () Int)
(declare-fun GHC.Types.Int32Rep () Int)
(declare-fun lq_tmp$36$x$35$$35$3174 () Int)
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
(assert (distinct GHC.Tuple.$40$$41$ GHC.Classes.C$58$$40$$37$$37$$41$))
(assert (distinct GHC.Types.False GHC.Types.True))

(assert (distinct lit$36$$123$ lit$36$MaximumCity$45$0.9.9$45$IUoVqdUkwWmEGL6yhjWbHS lit$36$UserInput lit$36$UseCases.AgricultureUseCase))
(assert (distinct GHC.Types.Int32Rep GHC.Types.Int16Rep GHC.Types.LiftedRep GHC.Types.FloatRep GHC.Types.AddrRep GHC.Types.Int64Rep GHC.Types.WordRep GHC.Types.Int8Rep GHC.Types.DoubleRep GHC.Types.IntRep GHC.Types.UnliftedRep GHC.Types.Word64Rep GHC.Types.Word16Rep GHC.Types.Word8Rep GHC.Types.Word32Rep))


(assert (distinct GHC.Types.GT GHC.Types.EQ GHC.Types.LT))
(assert (= (strLen lit$36$UseCases.AgricultureUseCase) 27))
(assert (= (strLen lit$36$UserInput) 9))
(assert (= (strLen lit$36$MaximumCity$45$0.9.9$45$IUoVqdUkwWmEGL6yhjWbHS) 40))
(assert (= (strLen lit$36$$123$) 1))
(push 1)
(push 1)
(pop 1)
(pop 1)
(exit)