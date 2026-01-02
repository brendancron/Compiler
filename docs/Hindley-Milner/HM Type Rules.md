Top part - premise
Bottom part - conclusion
## Rule 1: Variable Rule
$$
\frac{x : \sigma \in \Gamma}{\Gamma \vdash x : \sigma}
$$
If the variable is type σ in the context, it can be inferred from the context that x is type σ

## Rule 2: Function Application Rule
$$
\frac{
  \Gamma \vdash e_0 : \tau_a \to \tau_b
  \quad
  \Gamma \vdash e_1 : \tau_a
}{
  \Gamma \vdash e_0\ e_1 : \tau_b
}
$$
If e0 has type ta -> tb and e1 has type ta THEN e0 e1 has type tb.

## Rule 3: Function Abstraction Rule
$$
\frac{
  \Gamma, x : \tau_a \vdash e : \tau_b
}{
  \Gamma \vdash \lambda x \to e : \tau_a \to \tau_b
}
$$
If x has type ta and e has type tb THEN λx -> e has type ta -> tb

## Rule 4: Let Binding Rule
$$
\frac{
  \Gamma \vdash e_0 : \sigma
  \quad
  \Gamma, x : \sigma \vdash e_1 : \tau
}{
  \Gamma \vdash \text{let } x = e_0 \text{ in } e_1 : \tau
}
$$
If e has type σ in the context and if we add x having type σ in the context, e1 has type t THEN let x = e0 in e1 has type t.

## Rule 5: Instantiation Typing Rule
See [[Type Order in HM#Instantiation]]
$$
\frac{
  \Gamma \vdash e : \sigma_a
  \quad
  \sigma_a \sqsubseteq \sigma_b
}{
  \Gamma \vdash e : \sigma_b
}
$$
If an expression `e` has type `σₐ`, and `σₐ` is more specific than `σᵦ`, then `e` can also be treated as having type `σᵦ`.

## Rule 6: Generalization Typing Rule
See [[Type Order in HM#Generalization]]
$$
\frac{
  \Gamma \vdash e : \sigma
  \quad
  \alpha \notin FV(\Gamma)
}{
  \Gamma \vdash e : \forall \alpha.\ \sigma
}
$$
If `e` has type `σ`, and `α` is not a free variable in the context, then `e` can be generalized to a universally quantified type over `α`.