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
        \text{int\_lit}\\
        \text{string\_lit}\\
        \text{ident}\\
        \text{[BinExpr]}\\
        \text{([Expr])}\\
    \end{cases}\\

    [\text{BinExpr}] &\to \begin{cases}
        \text{[Expr]} + \text{[Expr]} & \text{prec = 0}\\
        \text{[Expr]} - \text{[Expr]} & \text{prec = 0}\\
        \text{[Expr]} \times \text{[Expr]} & \text{prec = 1}\\
        \text{[Expr]} \div \text{[Expr]} & \text{prec = 1}\\
    \end{cases}\\

\end{align}
$$