$$
\begin{align}

    [\text{Stmt}] &\to \begin{cases}
        [\text{Builtin}];\\
        \text{let ident = [Expr]};
    \end{cases}\\

    [\text{Builtin}] &\to \begin{cases}
        \text{exit([Expr])}\\
        \text{print([Expr])}
    \end{cases}\\

    [\text{Expr}] &\to \begin{cases}
        [\text{Term}]\\
    \end{cases}\\

    [\text{Term}] &\to \begin{cases}
        \text{int\_lit}\\
        \text{string\_lit}\\
        \text{ident}
    \end{cases}\\

\end{align}
$$