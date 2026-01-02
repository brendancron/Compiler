See [[HM Type Rules]]

```
W: TypeEnv * Expr -> Subst * Type
```

```
fn W(Γ, expr) : (Subst * Type) {
	match expr {
		Var x => {
			σ = Γ[x];
			return (∅, instantiate(σ));
		}
		Lambda (x, e) => {
			α = Γ.fresh();
			(S, τ) = W(Γ ∪ { x : α }, e)
			return (S, S(α) -> τ)
		}
		App(e0, e1) => {
			(S1, τ0) = W(Γ, e0)
			(S2, τ1) = W(S1(Γ), e1)
			α = Γ.fresh();
			S3 = unify(S2(τ0), τ1 -> α)
			return (S3 ∘ S2 ∘ S1, S3(α))
		}
		Let(x, e0, e1) => {
			(S1, τ0) = W(Γ, e0)
			σ = generalize(S1(Γ), τ0)
			(S2, τ1) = W(S1(Γ) ∪ { x : σ }, e1)
			return (S2 ∘ S1, τ1)
		}
		Literal / primitive => {
			return (∅, literal_type)
		}
	}
}

Need to implement instantiate and generalize!
```