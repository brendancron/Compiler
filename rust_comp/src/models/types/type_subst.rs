use crate::models::types::type_error::TypeError;
use crate::models::types::types::{Type, TypeVar};
use std::collections::HashMap;

pub type TypeSubst = HashMap<TypeVar, Type>;

pub trait ApplySubst {
    fn apply(&self, subst: &TypeSubst) -> Self;
}

impl ApplySubst for Type {
    fn apply(&self, subst: &TypeSubst) -> Type {
        match self {
            Type::Var(tv) => subst.get(tv).cloned().unwrap_or(self.clone()),
            Type::Func { params, ret } => Type::Func {
                params: params.iter().map(|t| t.apply(subst)).collect(),
                ret: Box::new(ret.apply(subst)),
            },
            _ => self.clone(),
        }
    }
}

pub fn unify(a: &Type, b: &Type, subst: &mut TypeSubst) -> Result<(), TypeError> {
    let a = a.apply(subst);
    let b = b.apply(subst);

    match (&a, &b) {
        (Type::Var(v), t) | (t, Type::Var(v)) => {
            subst.insert(*v, t.clone());
            Ok(())
        }
        (Type::Primitive(p1), Type::Primitive(p2)) if p1 == p2 => Ok(()),
        _ => Err(TypeError::TypeMismatch {
            expected: a,
            found: b,
        }),
    }
}
