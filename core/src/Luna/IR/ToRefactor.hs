{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoOverloadedStrings       #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.IR.ToRefactor where

import Luna.Prelude hiding (String, log, nested)
import qualified Luna.Prelude as Prelude

import Luna.IR.Internal.IR
import qualified Luna.IR.Expr.Term.Named as Term
import qualified Luna.IR.Internal.LayerStore as Store
import Luna.IR.Expr.Layout.Class
import Luna.IR.Expr.Layout.ENT
import Luna.IR.Layer
import Luna.IR.Layer.Type
import Luna.IR.Layer.Model
import Luna.IR.Layer.UID
import Luna.IR.Layer.Succs
import Luna.IR.Expr.Term.Named (HasName, name)
import Luna.IR.Expr.Format
import Luna.IR.Expr.Atom
import Data.Property
import qualified Luna.Pass        as Pass
import           Luna.Pass        (Pass, Preserves, Inputs, Outputs, Events, SubPass, Uninitialized, Template, DynPass, ElemScope, KnownElemPass, elemPassDescription, genericDescription, genericDescription')
import Data.TypeDesc
import Data.Event (type (//), Tag(Tag))
import qualified Data.Set as Set
import Luna.IR.Internal.LayerStore (STRefM)
import Luna.IR.Expr
import Unsafe.Coerce (unsafeCoerce)
import Luna.Pass.Manager as PM
import Data.Event as Event
import System.Log
import qualified Control.Monad.State.Dependent.Old as DepState

import qualified GHC.Prim as Prim

import Data.Reflection (Reifies)
import Luna.Pass.TH

import Type.Any (AnyType)

---------------------------------------
-- Some important utils





type family RTC (c :: Constraint) :: Constraint where
    RTC () = ()
    RTC (t1,t2) = (RTC t1, (RTC t2, ()))
    RTC (t1,t2,t3) = (RTC t1, (RTC t2, (RTC t3, ())))
    RTC (t1,t2,t3,t4) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, ()))))
    RTC (t1,t2,t3,t4,t5) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, ())))))
    RTC (t1,t2,t3,t4,t5,t6) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, ()))))))
    RTC (t1,t2,t3,t4,t5,t6,t7) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, ())))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, ()))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, ())))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, ()))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, ())))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, ()))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, ())))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, ()))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, ())))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, ()))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, ())))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, ()))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, ())))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, ()))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, ())))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, ()))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, ())))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, ()))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, ())))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, ()))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, ())))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, ()))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, ())))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, ()))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, ())))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, ()))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, ())))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, ()))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, ())))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, ()))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, ())))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, ()))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, ())))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, (RTC t40, ()))))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, (RTC t40, (RTC t41, ())))))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, (RTC t40, (RTC t41, (RTC t42, ()))))))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, (RTC t40, (RTC t41, (RTC t42, (RTC t43, ())))))))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, (RTC t40, (RTC t41, (RTC t42, (RTC t43, (RTC t44, ()))))))))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, (RTC t40, (RTC t41, (RTC t42, (RTC t43, (RTC t44, (RTC t45, ())))))))))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, (RTC t40, (RTC t41, (RTC t42, (RTC t43, (RTC t44, (RTC t45, (RTC t46, ()))))))))))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, (RTC t40, (RTC t41, (RTC t42, (RTC t43, (RTC t44, (RTC t45, (RTC t46, (RTC t47, ())))))))))))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, (RTC t40, (RTC t41, (RTC t42, (RTC t43, (RTC t44, (RTC t45, (RTC t46, (RTC t47, (RTC t48, ()))))))))))))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, (RTC t40, (RTC t41, (RTC t42, (RTC t43, (RTC t44, (RTC t45, (RTC t46, (RTC t47, (RTC t48, (RTC t49, ())))))))))))))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, (RTC t40, (RTC t41, (RTC t42, (RTC t43, (RTC t44, (RTC t45, (RTC t46, (RTC t47, (RTC t48, (RTC t49, (RTC t50, ()))))))))))))))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50,t51) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, (RTC t40, (RTC t41, (RTC t42, (RTC t43, (RTC t44, (RTC t45, (RTC t46, (RTC t47, (RTC t48, (RTC t49, (RTC t50, (RTC t51, ())))))))))))))))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50,t51,t52) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, (RTC t40, (RTC t41, (RTC t42, (RTC t43, (RTC t44, (RTC t45, (RTC t46, (RTC t47, (RTC t48, (RTC t49, (RTC t50, (RTC t51, (RTC t52, ()))))))))))))))))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50,t51,t52,t53) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, (RTC t40, (RTC t41, (RTC t42, (RTC t43, (RTC t44, (RTC t45, (RTC t46, (RTC t47, (RTC t48, (RTC t49, (RTC t50, (RTC t51, (RTC t52, (RTC t53, ())))))))))))))))))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50,t51,t52,t53,t54) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, (RTC t40, (RTC t41, (RTC t42, (RTC t43, (RTC t44, (RTC t45, (RTC t46, (RTC t47, (RTC t48, (RTC t49, (RTC t50, (RTC t51, (RTC t52, (RTC t53, (RTC t54, ()))))))))))))))))))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50,t51,t52,t53,t54,t55) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, (RTC t40, (RTC t41, (RTC t42, (RTC t43, (RTC t44, (RTC t45, (RTC t46, (RTC t47, (RTC t48, (RTC t49, (RTC t50, (RTC t51, (RTC t52, (RTC t53, (RTC t54, (RTC t55, ())))))))))))))))))))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50,t51,t52,t53,t54,t55,t56) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, (RTC t40, (RTC t41, (RTC t42, (RTC t43, (RTC t44, (RTC t45, (RTC t46, (RTC t47, (RTC t48, (RTC t49, (RTC t50, (RTC t51, (RTC t52, (RTC t53, (RTC t54, (RTC t55, (RTC t56, ()))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50,t51,t52,t53,t54,t55,t56,t57) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, (RTC t40, (RTC t41, (RTC t42, (RTC t43, (RTC t44, (RTC t45, (RTC t46, (RTC t47, (RTC t48, (RTC t49, (RTC t50, (RTC t51, (RTC t52, (RTC t53, (RTC t54, (RTC t55, (RTC t56, (RTC t57, ())))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50,t51,t52,t53,t54,t55,t56,t57,t58) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, (RTC t40, (RTC t41, (RTC t42, (RTC t43, (RTC t44, (RTC t45, (RTC t46, (RTC t47, (RTC t48, (RTC t49, (RTC t50, (RTC t51, (RTC t52, (RTC t53, (RTC t54, (RTC t55, (RTC t56, (RTC t57, (RTC t58, ()))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50,t51,t52,t53,t54,t55,t56,t57,t58,t59) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, (RTC t40, (RTC t41, (RTC t42, (RTC t43, (RTC t44, (RTC t45, (RTC t46, (RTC t47, (RTC t48, (RTC t49, (RTC t50, (RTC t51, (RTC t52, (RTC t53, (RTC t54, (RTC t55, (RTC t56, (RTC t57, (RTC t58, (RTC t59, ())))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50,t51,t52,t53,t54,t55,t56,t57,t58,t59,t60) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, (RTC t40, (RTC t41, (RTC t42, (RTC t43, (RTC t44, (RTC t45, (RTC t46, (RTC t47, (RTC t48, (RTC t49, (RTC t50, (RTC t51, (RTC t52, (RTC t53, (RTC t54, (RTC t55, (RTC t56, (RTC t57, (RTC t58, (RTC t59, (RTC t60, ()))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50,t51,t52,t53,t54,t55,t56,t57,t58,t59,t60,t61) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, (RTC t40, (RTC t41, (RTC t42, (RTC t43, (RTC t44, (RTC t45, (RTC t46, (RTC t47, (RTC t48, (RTC t49, (RTC t50, (RTC t51, (RTC t52, (RTC t53, (RTC t54, (RTC t55, (RTC t56, (RTC t57, (RTC t58, (RTC t59, (RTC t60, (RTC t61, ())))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    RTC (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50,t51,t52,t53,t54,t55,t56,t57,t58,t59,t60,t61,t62) = (RTC t1, (RTC t2, (RTC t3, (RTC t4, (RTC t5, (RTC t6, (RTC t7, (RTC t8, (RTC t9, (RTC t10, (RTC t11, (RTC t12, (RTC t13, (RTC t14, (RTC t15, (RTC t16, (RTC t17, (RTC t18, (RTC t19, (RTC t20, (RTC t21, (RTC t22, (RTC t23, (RTC t24, (RTC t25, (RTC t26, (RTC t27, (RTC t28, (RTC t29, (RTC t30, (RTC t31, (RTC t32, (RTC t33, (RTC t34, (RTC t35, (RTC t36, (RTC t37, (RTC t38, (RTC t39, (RTC t40, (RTC t41, (RTC t42, (RTC t43, (RTC t44, (RTC t45, (RTC t46, (RTC t47, (RTC t48, (RTC t49, (RTC t50, (RTC t51, (RTC t52, (RTC t53, (RTC t54, (RTC t55, (RTC t56, (RTC t57, (RTC t58, (RTC t59, (RTC t60, (RTC t61, (RTC t62, ()))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    RTC t1 = (t1, ())


type family FilterInputs t (a :: Constraint) :: [*] where
    FilterInputs t (a,as)         = FilterInputs t a <> FilterInputs t as
    FilterInputs t (Reader t a m) = '[a]
    FilterInputs t a              = '[]

type family FilterOutputs t (a :: Constraint) :: [*] where
    FilterOutputs t (a,as)         = FilterOutputs t a <> FilterOutputs t as
    FilterOutputs t (Reader t a m) = '[a]
    FilterOutputs t a              = '[]

type family FilterEmitters (a :: Constraint) :: [*] where
    FilterEmitters (a,as)        = FilterEmitters a <> FilterEmitters as
    FilterEmitters (Emitter e m) = '[e]
    FilterEmitters a             = '[]

type GetInputs   t c = FilterInputs   t (RTC c)
type GetOutputs  t c = FilterOutputs  t (RTC c)
type GetEmitters e   = FilterEmitters   (RTC e)

-- type Foo m = ( Readers Layer '[AnyExpr // Model, AnyExpr // Type] m
--              , Editors Net   '[AnyExprLink] m
--              , Emitter (Delete // AnyExprLink) m
--              , MonadRef m
--              )




-- type Foo m = Req m '[ Reader  // Layer  // AnyExpr // '[Model, Type]
--                     , Reader  // Net    // AnyExprLink
--                     , Emitter // Delete // AnyExprLink
--                     ]

type Req m (rs :: [*]) = (DescConstr m (Expands (Proxy rs)), MonadRef m)

-- type family Expand es ls where
--     Expand es ((l :: [k]) // ls) = es
--     Expand es ((l :: *)   // ls) = es



type family DescConstr m ls :: Constraint where
    DescConstr m '[] = ()
    DescConstr m ((Reader  // t // s) ': rs) = (Reader  t s m, DescConstr m rs)
    DescConstr m ((Writer  // t // s) ': rs) = (Writer  t s m, DescConstr m rs)
    DescConstr m ((Editor  // t // s) ': rs) = (Editor  t s m, DescConstr m rs)
    DescConstr m ((Emitter // e)      ': rs) = (Emitter e   m, DescConstr m rs)





-----------------------------------------------




type instance RefData Attr a _ = a





data Abstracted a
type instance Abstract (TypeRef s) = TypeRef (Abstracted s)




-- data ELEMSCOPE p elem
-- data ElemScope p elem
-- type instance Abstract (ElemScope c t) = ELEMSCOPE (Abstract c) (Abstract t)
--
-- type ElemSubPass p elem   = SubPass (ElemScope p elem)
-- type ElemPass    p elem m = ElemSubPass p elem m ()
--
-- proxifyElemPass :: ElemSubPass p elem m a -> (Proxy elem -> ElemSubPass p elem m a)
-- proxifyElemPass = const ; {-# INLINE proxifyElemPass #-}


-- m (m [Template (DynPass (GetPassManager m))])




-------------------------------------------
-------------------------------------------
-------------------------------------------
-------------------------------------------
-- Layer passes

type MonadPassManagerST m s = (MonadPassManager m, s ~ PrimState m)

debugPassRunning layer = withDebugBy ("Pass [" <> layer <> "]") "Running"



proxify :: a -> Proxy a
proxify _ = Proxy

newtype EventPass  p e t s = EventPass (forall m. MonadPassManagerST m s => PayloadData (e // t) -> Pass (ElemScope p t) m)
type    EventPassM p e t m = EventPass p e t (PrimState m)

runEventPass :: forall p e t m. (KnownType p, MonadPassManager m) => EventPassM p e t m -> PayloadData (e // t) -> Pass (ElemScope p t) m
runEventPass (EventPass f) pload = debugPassRunning (show $ getTypeDesc_ @p) $ f pload

foo :: forall p e t m. (KnownType p, MonadPassManager m, Pass.PassInit (ElemScope p t) m) => EventPassM p e t m -> Pass.Describbed (Uninitialized m (Template (DynPass m)))
foo = Pass.describbed @(ElemScope p t) . Pass.compileTemplate . Pass.template . runEventPass



newtype EventPassDef e t m = EventPassDef (PayloadData (e // t) -> m ())



                -- => Template (SubPass pass m a) -> Uninitialized m (Template (DynSubPass m a))

-- newtype NewElemPassDef p m = NewElemPassDef (forall t. KnownType (Abstract t) => (Elem t, Definition t) -> Pass (ElemScope p (Elem t)) m)
newtype NewElemPassDef t m = NewElemPassDef (EventPassDef New (Elem t) m)
newtype NewElemPass p s = NewElemPass (forall t m. (KnownType (Abstract t), MonadPassManager m, s ~ PrimState m) => NewElemPassDef t (SubPass (ElemScope p t) m))


runNewElemPassDef :: NewElemPassDef t m -> PayloadData (New // Elem t) -> m ()
runNewElemPassDef (NewElemPassDef (EventPassDef f)) p = f p

newElemPassDef :: (PayloadData (New // Elem t) -> m ()) -> NewElemPassDef t m
newElemPassDef f = NewElemPassDef $ EventPassDef f ; {-# INLINE newElemPassDef #-}

-- registerGenLayer2 :: (MonadPassManager m, MonadPassManager (GetRefHandler m), Pass.DataLookup (GetRefHandler m), KnownElemPass p, KnownType p) => LayerDesc -> NewElemPassDef (Pass (ElemScope p) ) -> m ()
-- registerGenLayer2 l p = registerLayerProto l $ prepareProto $ Pass.template $ runNewElemPassDef p ; {-# INLINE registerGenLayer2 #-}


newtype GenLayerCons  p s = GenLayerCons (forall t m. (KnownType (Abstract t), MonadPassManager m, s ~ PrimState m) => (Elem t, Definition t) -> Pass (ElemScope p (Elem t)) m)
type    GenLayerConsM p m = GenLayerCons p (PrimState m)

newtype ExprLayerCons  p s = ExprLayerCons (forall l m. MonadPassManagerST m s => (Expr l, Definition (Expr l)) -> Pass (ElemScope p (Expr l)) m)
type    ExprLayerConsM p m = ExprLayerCons p (PrimState m)

runGenLayerCons :: forall p m. (KnownType p, MonadPassManager m) => GenLayerConsM p m -> forall t. KnownType (Abstract t) => (Elem t, Definition t) -> Pass (ElemScope p (Elem t)) m
runGenLayerCons (GenLayerCons f) (t, tdef) = debugPassRunning (show $ getTypeDesc_ @p) $ f (t, tdef)

runExprLayerCons :: forall p m. (KnownType p, MonadPassManager m) => ExprLayerConsM p m -> forall l. (Expr l, Definition (Expr l)) -> Pass (ElemScope p (Expr l)) m
runExprLayerCons (ExprLayerCons f) (t, tdef) = debugPassRunning (show $ getTypeDesc_ @p) $ f (t, tdef)


registerGenLayer :: (MonadPassManager m, MonadPassManager (GetRefHandler m), Pass.DataLookup (GetRefHandler m), KnownElemPass p, KnownType p) => LayerDesc -> GenLayerConsM p (GetRefHandler m) -> m ()
registerGenLayer l p = registerLayerProto l $ prepareProto $ Pass.template $ runGenLayerCons p ; {-# INLINE registerGenLayer #-}


registerGenLayerM :: (MonadPassManager m, MonadPassManager (GetRefHandler m), KnownElemPass p, KnownType p, Pass.DataLookup (GetRefHandler m)) => LayerDesc -> m (GenLayerConsM p (GetRefHandler m)) -> m ()
registerGenLayerM l p = registerGenLayer l =<< p ; {-# INLINE registerGenLayerM #-}


registerExprLayer :: forall p l m. (MonadPassManager m, MonadPassManager (GetRefHandler m), KnownElemPass p, KnownType p, Pass.DataLookup (GetRefHandler m)) => LayerDesc -> ExprLayerConsM p (GetRefHandler m) -> m ()
registerExprLayer l p = registerLayerProto l $ Pass.Proto $ \_ -> Pass.describbed @(ElemScope p (Expr l)) . Pass.compileTemplate $ Pass.template $ runExprLayerCons p ; {-# INLINE registerExprLayer #-}

registerExprLayerM :: (MonadPassManager m, MonadPassManager (GetRefHandler m), KnownElemPass p, KnownType p, Pass.DataLookup (GetRefHandler m)) => LayerDesc -> m (ExprLayerConsM p (GetRefHandler m)) -> m ()
registerExprLayerM l p = registerExprLayer l =<< p ; {-# INLINE registerExprLayerM #-}



prepareProto :: forall p m. (Logging m, Pass.DataLookup m, KnownElemPass p) => (forall s. TypeReify (Abstracted s) => Template (Pass (ElemScope p (Elem (TypeRef s))) m)) -> Pass.Proto (Pass.Describbed (Uninitialized m (Template (DynPass m))))
prepareProto p = Pass.Proto $ reifyKnownTypeT @Abstracted (prepareProto' p) . (head . view subDescs) {- we take type args here, cause we need only `t` instead of `Elem t` -} where
    prepareProto' :: forall p t m. (Logging m, KnownType (Abstract t), Pass.DataLookup m, KnownElemPass p) => Template (Pass (ElemScope p (Elem t)) m) -> Proxy t -> Pass.Describbed (Uninitialized m (Template (DynPass m)))
    prepareProto' = const . Pass.describbed @(ElemScope p (Elem t)) . Pass.compileTemplate



-------------------
-- === Model === --
-------------------


data InitModel
type instance Abstract InitModel = InitModel
type instance Inputs  Net   (ElemScope InitModel t) = '[]
type instance Outputs Net   (ElemScope InitModel t) = '[]
type instance Inputs  Layer (ElemScope InitModel t) = '[]
type instance Outputs Layer (ElemScope InitModel t) = '[Abstract t // Model]
type instance Inputs  Attr  (ElemScope InitModel t) = '[]
type instance Outputs Attr  (ElemScope InitModel t) = '[]
type instance Inputs  Event (ElemScope InitModel t) = '[]
type instance Outputs Event (ElemScope InitModel t) = '[]
type instance Preserves     (ElemScope InitModel t) = '[]
instance KnownElemPass InitModel where
    elemPassDescription = genericDescription' . proxify

initModel :: GenLayerCons InitModel s
initModel = GenLayerCons $ uncurry $ flip $ writeLayer @Model ; {-# INLINE initModel #-}




type family PassType p d
type instance PassType p (NewElemPassDef t _) = ElemScope p t

data InitModel2
type instance Abstract InitModel2 = InitModel2
type instance Inputs  Net   (ElemScope InitModel2 t) = '[]
type instance Outputs Net   (ElemScope InitModel2 t) = '[]
type instance Inputs  Layer (ElemScope InitModel2 t) = '[]
type instance Outputs Layer (ElemScope InitModel2 t) = '[Elem (Abstract t) // Model]
type instance Inputs  Attr  (ElemScope InitModel2 t) = '[]
type instance Outputs Attr  (ElemScope InitModel2 t) = '[]
type instance Inputs  Event (ElemScope InitModel2 t) = '[]
type instance Outputs Event (ElemScope InitModel2 t) = '[]
type instance Preserves     (ElemScope InitModel2 t) = '[]
instance KnownElemPass InitModel2 where
    elemPassDescription = genericDescription' . proxify


initModel2 :: Req m '[Writer // Layer // Elem (Abstract t) // Model] => NewElemPassDef t m
initModel2 = newElemPassDef . uncurry . flip $ writeLayer @Model ; {-# INLINE initModel2 #-}

-- initModel3 :: (Writer Layer (Elem (Abstract t) // Model) m, MonadRef m) => PayloadData (New // Elem t) -> m ()
-- initModel3 = runNewElemPassDef initModel2
--
-- initModel4 :: (EqPrims m (GetRefHandler m), MonadRef m) => PayloadData (New // Elem t) -> Pass (ElemScope InitModel2 t) m
-- initModel4 = initModel3
--
-- initmodel5 :: NewElemPass InitModel2 s
-- initmodel5 = NewElemPass initModel4

initmodelPass :: NewElemPass InitModel2 s
initmodelPass = NewElemPass initModel2




-- initModel :: Req m '[Writer // Layer // Elem (Abstract t) // Model] => NewElemPassDef t m
-- initModel = newElemPassDef . uncurry . flip $ writeLayer @Model ; {-# INLINE initModel #-}
-- makePass 'initModel


-----------------
-- === UID === --
-----------------

data InitUID
type instance Abstract InitUID = InitUID
type instance Inputs  Net   (ElemScope InitUID t) = '[]
type instance Outputs Net   (ElemScope InitUID t) = '[]
type instance Inputs  Layer (ElemScope InitUID t) = '[]
type instance Outputs Layer (ElemScope InitUID t) = '[Abstract t // UID]
type instance Inputs  Attr  (ElemScope InitUID t) = '[]
type instance Outputs Attr  (ElemScope InitUID t) = '[]
type instance Inputs  Event (ElemScope InitUID t) = '[]
type instance Outputs Event (ElemScope InitUID t) = '[]
type instance Preserves     (ElemScope InitUID t) = '[]
instance KnownElemPass InitUID where
    elemPassDescription = genericDescription' . proxify


initUID :: PrimMonad m => m (GenLayerConsM InitUID m)
initUID = do
    ref <- Store.newSTRef (def :: ID)
    return $ GenLayerCons $ \(t, tdef) -> do
        nuid <- Store.modifySTRef' ref (\i -> (i, succ i))
        writeLayer @UID nuid t
{-# INLINE initUID #-}


-------------------
-- === Succs === --
-------------------

data InitSuccs
type instance Abstract InitSuccs = InitSuccs
type instance Inputs  Net   (ElemScope InitSuccs t) = '[]
type instance Outputs Net   (ElemScope InitSuccs t) = '[]
type instance Inputs  Layer (ElemScope InitSuccs t) = '[]
type instance Outputs Layer (ElemScope InitSuccs t) = '[Abstract t // Succs]
type instance Inputs  Attr  (ElemScope InitSuccs t) = '[]
type instance Outputs Attr  (ElemScope InitSuccs t) = '[]
type instance Inputs  Event (ElemScope InitSuccs t) = '[]
type instance Outputs Event (ElemScope InitSuccs t) = '[]
type instance Preserves     (ElemScope InitSuccs t) = '[]
instance KnownElemPass InitSuccs where
    elemPassDescription = genericDescription' . proxify


initSuccs :: GenLayerCons InitSuccs s
initSuccs = GenLayerCons $ \(t, _) -> writeLayer @Succs mempty t ; {-# INLINE initSuccs #-}


data WatchSuccs
type instance Abstract WatchSuccs = WatchSuccs
type instance Inputs  Net   (ElemScope WatchSuccs t) = '[]
type instance Outputs Net   (ElemScope WatchSuccs t) = '[]
type instance Inputs  Layer (ElemScope WatchSuccs t) = '[AnyExpr // Succs]
type instance Outputs Layer (ElemScope WatchSuccs t) = '[AnyExpr // Succs]
type instance Inputs  Attr  (ElemScope WatchSuccs t) = '[]
type instance Outputs Attr  (ElemScope WatchSuccs t) = '[]
type instance Inputs  Event (ElemScope WatchSuccs t) = '[]
type instance Outputs Event (ElemScope WatchSuccs t) = '[]
type instance Preserves     (ElemScope WatchSuccs t) = '[]
instance KnownElemPass WatchSuccs where
    elemPassDescription = genericDescription' . proxify

watchSuccs :: EventPass WatchSuccs New (ExprLink' l) s
watchSuccs = EventPass $ \(t, (src, tgt)) -> modifyLayer_ @Succs (Set.insert $ unsafeGeneralize t) src ; {-# INLINE watchSuccs #-}
    -- debugElem t $ "New successor: " <> show (src ^. idx) <> " -> " <> show (tgt ^. idx)


data WatchRemoveEdge
type instance Abstract WatchRemoveEdge = WatchRemoveEdge
type instance Inputs  Net   (ElemScope WatchRemoveEdge t) = '[]
type instance Outputs Net   (ElemScope WatchRemoveEdge t) = '[]
type instance Inputs  Layer (ElemScope WatchRemoveEdge t) = '[AnyExpr // Succs, AnyExprLink // Model]
type instance Outputs Layer (ElemScope WatchRemoveEdge t) = '[AnyExpr // Succs]
type instance Inputs  Attr  (ElemScope WatchRemoveEdge t) = '[]
type instance Outputs Attr  (ElemScope WatchRemoveEdge t) = '[]
type instance Inputs  Event (ElemScope WatchRemoveEdge t) = '[]
type instance Outputs Event (ElemScope WatchRemoveEdge t) = '[]
type instance Preserves     (ElemScope WatchRemoveEdge t) = '[]
instance KnownElemPass WatchRemoveEdge where
    elemPassDescription = genericDescription' . proxify

watchRemoveEdge :: EventPass WatchRemoveEdge Delete (ExprLink' l) s
watchRemoveEdge = EventPass $ \t -> do
    (src, tgt) <- readLayer @Model t
    modifyLayer_ @Succs (Set.delete $ unsafeGeneralize t) src
    -- debugElem t $ "Delete successor: " <> show (src ^. idx) <> " -> " <> show (tgt ^. idx)



--
--
-- data WatchRemoveNode
-- type instance Inputs  Net   (ElemScope WatchRemoveNode t) = '[AnyExprLink]
-- type instance Outputs Net   (ElemScope WatchRemoveNode t) = '[AnyExprLink]
-- type instance Inputs  Layer (ElemScope WatchRemoveNode t) = '[AnyExpr // Model, AnyExpr // Type]
-- type instance Outputs Event (ElemScope WatchRemoveNode t) = '[Delete // AnyExprLink]
-- makeEventPass ''WatchRemoveNode

--
--
--
-- data WatchRemoveNode
-- type instance Req (ElemScope WatchRemoveNode t) = '[ Editor // Net // AnyExprLink
--                                                    , Input // Layer // '[Model, Type]
--                                                    , Event // Delete // AnyExprLink
--                                                    ]
-- makeEventPass ''WatchRemoveNode
--
-- watchRemoveNode :: EventPass WatchRemoveNode Delete (Expr l) s
-- watchRemoveNode = EventPass $ \t -> do
--     inps   <- symbolFields (generalize t :: SomeExpr)
--     tp     <- readLayer @Type t
--     delete tp
--     mapM_ delete inps
--
--
--
-- watchRemoveNode :: Req m '[ Editor // Net    // AnyExprLink
--                           , Input  // Layer  // '[Model, Type]
--                           , Event  // Delete // AnyExprLink
--                           ]
--                 => EventPass Delete (Expr l) m
-- watchRemoveNode = EventPass $ \t -> do
--     inps   <- symbolFields (generalize t :: SomeExpr)
--     tp     <- readLayer @Type t
--     delete tp
--     mapM_ delete inps
--
-- makeEventPass ''watchRemoveNode





watchRemoveNode2 :: ( Readers Layer '[AnyExpr // Model, AnyExpr // Type] m
                    , Editors Net   '[AnyExprLink] m
                    , Emitter (Delete // AnyExprLink) m
                    , MonadRef m
                    )
                 => EventPassDef Delete (Expr l) m
watchRemoveNode2 = EventPassDef $ \t -> do
    inps   <- symbolFields (generalize t :: SomeExpr)
    tp     <- readLayer @Type t
    delete tp
    mapM_ delete inps



data WatchRemoveNode
type instance Abstract WatchRemoveNode = WatchRemoveNode
type instance Inputs  Net   (ElemScope WatchRemoveNode t) = GetInputs  Net   (Foo AnyType)
type instance Outputs Net   (ElemScope WatchRemoveNode t) = GetOutputs Net   (Foo AnyType)
type instance Inputs  Layer (ElemScope WatchRemoveNode t) = GetInputs  Layer (Foo AnyType)
type instance Outputs Layer (ElemScope WatchRemoveNode t) = GetOutputs Layer (Foo AnyType)
type instance Inputs  Attr  (ElemScope WatchRemoveNode t) = GetInputs  Attr  (Foo AnyType)
type instance Outputs Attr  (ElemScope WatchRemoveNode t) = GetOutputs Attr  (Foo AnyType)
type instance Inputs  Event (ElemScope WatchRemoveNode t) = '[]
type instance Outputs Event (ElemScope WatchRemoveNode t) = GetEmitters (Foo AnyType)
type instance Preserves     (ElemScope WatchRemoveNode t) = '[]
instance KnownElemPass WatchRemoveNode where
    elemPassDescription = genericDescription' . proxify





type Foo m = Req m '[ Reader  // Layer  // AnyExpr // '[Model, Type]
                    , Editor  // Net    // AnyExprLink
                    , Emitter // Delete // AnyExprLink
                    ]

watchRemoveNode :: Foo m => EventPassDef Delete (Expr l) m
watchRemoveNode = EventPassDef $ \t -> do
    inps   <- symbolFields (generalize t :: SomeExpr)
    tp     <- readLayer @Type t
    delete tp
    mapM_ delete inps

-- makePass 'watchRemoveNode


------------------
-- === Type === --
------------------

consTypeLayer :: (MonadRef m, Writers Net '[AnyExpr, Link' AnyExpr] m, Emitters '[New // Link' AnyExpr, New // AnyExpr] m)
              => Store.STRefM m (Maybe (Expr Star)) -> Expr t -> m (LayerData Type (Expr t))
consTypeLayer ref self = (`link` self) =<< unsafeRelayout <$> localTop ref ; {-# INLINE consTypeLayer #-}


localTop :: (MonadRef m, Writer Net AnyExpr m, Emitter (New // AnyExpr) m)
         => Store.STRefM m (Maybe (Expr Star)) -> m (Expr Star)
localTop ref = Store.readSTRef ref >>= \case
    Just t  -> return t
    Nothing -> do
        s <- reserveStar
        Store.writeSTRef ref $ Just s
        registerStar s
        Store.writeSTRef ref Nothing
        return s
{-# INLINE localTop #-}


data InitType
type instance Abstract InitType = InitType
type instance Inputs  Net   (ElemScope InitType t) = '[]
type instance Outputs Net   (ElemScope InitType t) = '[AnyExpr, Link' AnyExpr]
type instance Inputs  Layer (ElemScope InitType t) = '[]
type instance Outputs Layer (ElemScope InitType t) = '[Abstract t // Type]
type instance Inputs  Attr  (ElemScope InitType t) = '[]
type instance Outputs Attr  (ElemScope InitType t) = '[]
type instance Inputs  Event (ElemScope InitType t) = '[]
type instance Outputs Event (ElemScope InitType t) = '[New // AnyExpr, New // Link' AnyExpr]
type instance Preserves     (ElemScope InitType t) = '[]
instance KnownElemPass InitType where
    elemPassDescription = genericDescription' . proxify

initType :: PrimMonad m => m (ExprLayerConsM InitType m)
initType = do
    ref <- Store.newSTRef (Nothing :: Maybe (Expr Star))
    return $ ExprLayerCons $ \(el, _) -> do
        t <- consTypeLayer ref el
        flip (writeLayer @Type) el t
{-# INLINE initType #-}





-------------------------------------------
-------------------------------------------
-------------------------------------------
-------------------------------------------


runRegs :: (MonadPassManager m, MonadPassManager (GetRefHandler m), Pass.DataLookup (GetRefHandler m)) => m ()
runRegs = do
    runElemRegs

    -- f <- ff
    registerGenLayer  (getTypeDesc @Model) initModel
    registerGenLayerM (getTypeDesc @UID)   initUID
    registerGenLayer  (getTypeDesc @Succs) initSuccs

    registerExprLayerM (getTypeDesc @Type) initType

    attachLayer 0 (getTypeDesc @Model) (getTypeDesc @AnyExpr)
    attachLayer 0 (getTypeDesc @Model) (getTypeDesc @(Link' AnyExpr))
    attachLayer 5 (getTypeDesc @UID)   (getTypeDesc @AnyExpr)
    attachLayer 5 (getTypeDesc @UID)   (getTypeDesc @(Link' AnyExpr))
    attachLayer 5 (getTypeDesc @Succs) (getTypeDesc @AnyExpr)


    attachLayer 10 (getTypeDesc @Type) (getTypeDesc @AnyExpr)


    addEventListener 100 (Tag [getTypeDesc @New   , getTypeDesc @(Link' AnyExpr)]) $ foo watchSuccs
    addEventListener 100 (Tag [getTypeDesc @Delete, getTypeDesc @(Link' AnyExpr)]) $ foo watchRemoveEdge
    -- addEventListener 100 (Tag [getTypeDesc @Delete, getTypeDesc @(Link' AnyExpr)]) $ foo watchRemoveNode


-- === Elem reg defs === --

runElemRegs :: MonadIR m => m ()
runElemRegs = sequence_ [elemReg1, elemReg2, elemReg3]

elemReg1 :: MonadIR m => m ()
elemReg1 = registerElem @AnyExpr

elemReg2 :: MonadIR m => m ()
elemReg2 = registerElem @(Link' AnyExpr)

elemReg3 :: MonadIR m => m ()
elemReg3 = registerElem @(GROUP AnyExpr)


-- === Layer reg defs === --

layerRegs :: MonadIR m => [m ()]
layerRegs = [] -- [layerReg1, layerReg2, layerReg3, layerReg4]

runLayerRegs :: MonadIR m => m ()
runLayerRegs = sequence_ layerRegs





----------------------------------
----------------------------------
----------------------------------


source :: (MonadRef m, Reader Layer (Abstract (Link a b) // Model) m) => Link a b -> m a
source = fmap fst . readLayer @Model ; {-# INLINE source #-}

-- strName :: _ => _
strName v = getName v >>= \n -> match' n >>= \ (Term.Sym_String s) -> return s



-- === KnownExpr === --

type KnownExpr l m = (MonadRef m, Readers Layer '[AnyExpr // Model, Link' AnyExpr // Model] m) -- CheckAtomic (ExprHead l))

match' :: forall l m. KnownExpr l m => Expr l -> m (ExprHeadDef l)
match' = unsafeToExprTermDef @(ExprHead l)

modifyExprTerm :: forall l m. (KnownExpr l m, Writer Layer (AnyExpr // Model) m) => Expr l -> (ExprHeadDef l -> ExprHeadDef l) -> m ()
modifyExprTerm = unsafeModifyExprTermDef @(ExprHead l)

getSource :: KnownExpr l m => Lens' (ExprHeadDef l) (ExprLink a b) -> Expr l -> m (Expr a)
getSource f v = match' v >>= source . view f ; {-# INLINE getSource #-}


-- === KnownName === --

type       KnownName l m = (KnownExpr l m, HasName (ExprHeadDef l))
getName :: KnownName l m => Expr l -> m (Expr (Sub NAME l))
getName = getSource name ; {-# INLINE getName #-}







type family Head a

type instance Access AnyExpr (ENT e _ _) = e
type instance Access AnyExpr (E   e    ) = e
type instance Head (Atomic a) = Atomic a

type ExprHead l = Head (l # AnyExpr)
type ExprHeadDef l = ExprTermDef (ExprHead l) (Expr l)



---------- TRASH
------ TO BE DELETED WHEN POSSIBLE

instance MonadLogging m => MonadLogging (DepState.StateT a b m)
