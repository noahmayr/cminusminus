$$
\begin{align}

    [\text{Prog}] &\to [\text{Scope}]\\

    [\text{Scope}] &\to [\text{Stmt}]^*\\

    [\text{Stmt}] &\to \begin{cases}
        [\text{Builtin}];\\
        \text{let ident = [Expr]};\\
        \{\text{[Scope]}\};\\
    \end{cases}\\

    [\text{Builtin}] &\to \begin{cases}
        \text{exit([Expr])}\\
        \text{print([Expr])}\\
    \end{cases}\\

    [\text{Expr}] &\to \begin{cases}
        [\text{Term}]\\
    \end{cases}\\

    [\text{Term}] &\to \begin{cases}
        \text{int\_lit}\\
        \text{string\_lit}\\
        \text{ident}\\
    \end{cases}\\

\end{align}
$$