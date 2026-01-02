See [[Unifying Substitutions]] and [[HM Type System]]
```
unify(a: MonoType, b: MonoType): Substitution {
	if a is a type_var {
		if b is the same type_var {
			// here we don't really need to do anything
			return {};
		} else if b contains a {
			throw error "occurs check failed, cannot create infinite type";
		}
		// simply map a to b
		return {a |-> b};
	}
	if b is a type_var {
		return unify(b, a);
	}
	if a and b are both type_func_app {
		if a and b are different type functions {
			throw error "failed to unify, different type functions";
		}
		// here just unify each argument piecewise
		let S = {};
		for i in range(number of type function arguments) {
			S = combine(S, unify(S(a.arguments[i]), S(b.arguments[i])));
		}
		return S;
	}
}	
```

