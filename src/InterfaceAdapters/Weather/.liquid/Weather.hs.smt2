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
(declare-fun GHC.Real.divMod () Int)
(declare-fun GHC.List.reverse () Int)
(declare-fun InterfaceAdapters.Preferences.Monsoon () Int)
(declare-fun GHC.List.scanl () Int)
(declare-fun GHC.Classes.min () Int)
(declare-fun GHC.Int.I64$35$ () Int)
(declare-fun GHC.Types.C$35$ () Int)
(declare-fun GHC.Real.$47$ () Int)
(declare-fun Data.ByteString.UTF8.toString () Int)
(declare-fun InterfaceAdapters.Preferences.Standard () Int)
(declare-fun GHC.Types.$58$ () Int)
(declare-fun tail () Int)
(declare-fun GHC.IO.Handle.Types.$36$WFileHandle () Int)
(declare-fun Data.ByteString.Lazy.Internal.Chunk () Int)
(declare-fun Data.Aeson.Types.ToJSON.C$58$ToJSON () Int)
(declare-fun isJust () Int)
(declare-fun InterfaceAdapters.Weather.PirateWeatherAPI.weatherCurrentForecast () Int)
(declare-fun GHC.Real.div () Int)
(declare-fun InterfaceAdapters.Preferences.RightNow () Int)
(declare-fun GHC.Classes.$62$$61$ () Int)
(declare-fun GHC.IO.Handle.Types.DuplexHandle () Int)
(declare-fun GHC.List.span () Int)
(declare-fun GHC.IO.Handle.Types.FileHandle () Int)
(declare-fun InterfaceAdapters.Utils.ShortCircuit.C$58$HasFalse () Int)
(declare-fun GHC.Tuple.$40$$44$$41$ () Int)
(declare-fun GHC.Real.$94$ () Int)
(declare-fun GHC.Classes.$61$$61$ () Int)
(declare-fun GHC.List.$33$$33$ () Int)
(declare-fun GHC.Base.$36$ () Int)
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
(declare-fun GHC.Generics.C$58$Generic () Int)
(declare-fun GHC.Base.pure () Int)
(declare-fun GHC.Base.$62$$62$$61$ () Int)
(declare-fun GHC.Real.fromIntegral () Int)
(declare-fun GHC.IO.Exception.IOError () Int)
(declare-fun stringlen () Int)
(declare-fun GHC.List.tail () Int)
(declare-fun GHC.Types.Module () Int)
(declare-fun GHC.Stack.Types.FreezeCallStack () Int)
(declare-fun GHC.Types.TrNameD () Int)
(declare-fun fromJust () Int)
(declare-fun InterfaceAdapters.Preferences.WaterLevels () Int)
(declare-fun Data.Aeson.Types.Internal.Array () Int)
(declare-fun Data.Foldable.length () Int)
(declare-fun Control.Exception.Base.patError () Int)
(declare-fun GHC.Classes.not () Int)
(declare-fun GHC.List.iterate () Int)
(declare-fun Data.Text.Internal.Text () Int)
(declare-fun Data.Text.$36$fEqText () Int)
(declare-fun Data.ByteString.Internal.PS () Int)
(declare-fun addrLen () Int)
(declare-fun GHC.Real.$36$W$58$$37$ () Int)
(declare-fun Data.Text.Show.unpack () Int)
(declare-fun GHC.List.splitAt () Int)
(declare-fun InterfaceAdapters.Preferences.WeatherWaterLevels () Int)
(declare-fun UseCases.WWI.UserAsk () Int)
(declare-fun papp3 () Int)
(declare-fun GHC.List.scanl1 () Int)
(declare-fun GHC.Real.toInteger () Int)
(declare-fun Data.Foldable.null () Int)
(declare-fun Data.Aeson.Types.Internal.Bool () Int)
(declare-fun Data.Text.pack () Int)
(declare-fun GHC.Classes.$38$$38$ () Int)
(declare-fun GHC.Classes.$124$$124$ () Int)
(declare-fun GHC.List.dropWhile () Int)
(declare-fun Data.Aeson.Types.Internal.String () Int)
(declare-fun GHC.Enum.C$58$Bounded () Int)
(declare-fun InterfaceAdapters.Preferences.All () Int)
(declare-fun GHC.List.init () Int)
(declare-fun GHC.List.scanr1 () Int)
(declare-fun GHC.Types.True () Bool)
(declare-fun GHC.CString.unpackCString$35$ () Int)
(declare-fun GHC.List.break () Int)
(declare-fun Data.Maybe.maybe () Int)
(declare-fun InterfaceAdapters.Utils.ShortCircuit.C$58$Shortcircuit () Int)
(declare-fun GHC.Stack.Types.EmptyCallStack () Int)
(declare-fun x_Tuple33 () Int)
(declare-fun Data.Either.Right () Int)
(declare-fun Data.Aeson.Types.FromJSON.C$58$FromJSON () Int)
(declare-fun GHC.Show.C$58$Show () Int)
(declare-fun cast_as () Int)
(declare-fun Data.Text.Internal.$36$WText () Int)
(declare-fun GHC.List.head () Int)
(declare-fun Data.ByteString.Lazy.Internal.$36$WChunk () Int)
(declare-fun GHC.Base.$36$fApplicativeIO () Int)
(declare-fun cast_as_int () Int)
(declare-fun GHC.Num.$43$ () Int)
(declare-fun GHC.Base.$36$fMonadIO () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WObject () Int)
(declare-fun len () Int)
(declare-fun GHC.Types.LT () Int)
(declare-fun GHC.Tuple.$40$$44$$44$$41$ () Int)
(declare-fun papp1 () Int)
(declare-fun GHC.List.drop () Int)
(declare-fun papp6 () Int)
(declare-fun GHC.Real.mod () Int)
(declare-fun GHC.Classes.C$58$IP () Int)
(declare-fun Data.Either.Left () Int)
(declare-fun GHC.Classes.C$58$Eq () Int)
(declare-fun lit$36$Pattern$32$match$40$es$41$$32$are$32$non$45$exhaustive () Str)
(declare-fun GHC.List.zip () Int)
(declare-fun liquid_internal_this () Int)
(declare-fun GHC.Real.fromRational () Int)
(declare-fun InterfaceAdapters.Preferences.LongRange () Int)
(declare-fun GHC.Classes.$62$ () Int)
(declare-fun GHC.Tuple.$40$$41$ () Int)
(declare-fun lqdc$35$$35$$36$select$35$$35$GHC.Maybe.Just$35$$35$1 () Int)
(declare-fun GHC.Types.I$35$ () Int)
(declare-fun Data.ByteString.Lazy.Internal.Empty () Int)
(declare-fun x_Tuple31 () Int)
(declare-fun InterfaceAdapters.Preferences.Weather () Int)
(declare-fun GHC.Num.Integer.IS () Int)
(declare-fun GHC.Real.quot () Int)
(declare-fun Data.Tuple.fst () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WArray () Int)
(declare-fun Data.Aeson.Types.Internal.Null () Int)
(declare-fun Data.Tuple.snd () Int)
(declare-fun InterfaceAdapters.Utils.HttpHeadersPathDefinitions.extractXForwardedForHeader () Int)
(declare-fun InterfaceAdapters.Water.MH.Core.WaterLevelHeaders.$36$sel$58$percent_Today$58$PercentLiveStorage () Int)
(declare-fun GHC.Types.EQ () Int)
(declare-fun InterfaceAdapters.Preferences.Mini () Int)
(declare-fun GHC.Real.$58$$37$ () Int)
(declare-fun fst () Int)
(declare-fun GHC.List.takeWhile () Int)
(declare-fun InterfaceAdapters.Preferences.Alerts () Int)
(declare-fun GHC.Num.negate () Int)
(declare-fun GHC.Num.$45$ () Int)
(declare-fun GHC.Real.rem () Int)
(declare-fun GHC.Classes.max () Int)
(declare-fun GHC.List.scanr () Int)
(declare-fun GHC.Real.recip () Int)
(declare-fun Data.Text.$36$fIsStringText () Int)
(declare-fun autolen () Int)
(declare-fun GHC.List.replicate () Int)
(declare-fun GHC.Base.. () Int)
(declare-fun GHC.List.take () Int)
(declare-fun InterfaceAdapters.Preferences.Preferences () Int)
(declare-fun papp4 () Int)
(declare-fun GHC.Base.return () Int)
(declare-fun InterfaceAdapters.Weather.OpenWeatherAPI.getWeatherForTown () Int)
(declare-fun Data.Aeson.Types.Internal.$36$WString () Int)
(declare-fun GHC.Stack.Types.PushCallStack () Int)
(declare-fun InterfaceAdapters.Water.MH.Core.WaterLevelLakes.getWaterLakeLevelForPlace_LiveToday_wrtStorage () Int)
(declare-fun GHC.Real.quotRem () Int)
(declare-fun GHC.Classes.$60$$61$ () Int)
(declare-fun InterfaceAdapters.Water.MH.Core.WaterLevelHeaders.PercentLiveStorage () Int)
(declare-fun GHC.Num.fromInteger () Int)
(declare-fun GHC.Classes.$60$ () Int)
(declare-fun GHC.List.filter () Int)
(declare-fun x_Tuple21 () Int)
(declare-fun GHC.List.repeat () Int)
(declare-fun head () Int)
(declare-fun GHC.Maybe.Nothing () Int)
(declare-fun GHC.Num.Integer.IP () Int)
(declare-fun GHC.Base.map () Int)
(declare-fun x_Tuple32 () Int)
(declare-fun Data.Aeson.Types.Internal.Number () Int)
(declare-fun InterfaceAdapters.Utils.ShortCircuit.C$58$HasTrue () Int)
(declare-fun Data.Text.isPrefixOf () Int)
(declare-fun GHC.Err.error () Int)
(declare-fun GHC.Classes.compare () Int)
(declare-fun GHC.Types.TrNameS () Int)
(declare-fun GHC.Num.Integer.IN () Int)
(declare-fun InterfaceAdapters.Preferences.Detailed () Int)
(declare-fun GHC.Num.$42$ () Int)
(declare-fun InterfaceAdapters.Weather.PirateWeatherAPI._getWeatherForTown () Int)
(declare-fun GHC.Types.False () Bool)
(declare-fun InterfaceAdapters.Preferences.NearForecast () Int)
(declare-fun papp2 () Int)
(declare-fun GHC.Classes.$47$$61$ () Int)
(declare-fun GHC.List.cycle () Int)
(declare-fun Data.String.fromString () Int)
(declare-fun GHC.IO.Handle.Types.$36$WDuplexHandle () Int)
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

(assert (distinct InterfaceAdapters.Preferences.Weather InterfaceAdapters.Preferences.All InterfaceAdapters.Preferences.WeatherWaterLevels InterfaceAdapters.Preferences.WaterLevels InterfaceAdapters.Preferences.Monsoon))




(assert (distinct GHC.Types.GT GHC.Types.EQ GHC.Types.LT))
(assert (distinct InterfaceAdapters.Preferences.Detailed InterfaceAdapters.Preferences.Mini InterfaceAdapters.Preferences.Standard))


(assert (distinct GHC.Types.False GHC.Types.True))
(assert (distinct InterfaceAdapters.Preferences.NearForecast InterfaceAdapters.Preferences.Alerts InterfaceAdapters.Preferences.LongRange InterfaceAdapters.Preferences.RightNow))

(assert (= (strLen lit$36$Pattern$32$match$40$es$41$$32$are$32$non$45$exhaustive) 36))
(push 1)
(push 1)
(pop 1)
(pop 1)
(exit)