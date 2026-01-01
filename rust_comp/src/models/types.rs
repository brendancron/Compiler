#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVar(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    String,
    Bool,

    Struct {
        name: String,
        fields: Vec<(String, Type)>,
    },

    Var(TypeVar),

    Func {
        params: Vec<Type>,
        ret: Box<Type>,
    },
}
