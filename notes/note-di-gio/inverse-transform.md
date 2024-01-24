---
mid: 1694505205907
nid: 1701627118640
tags: 
date: 2023-11-13 09:09
---

> Nota: metodo che data l'inversa della  [[CDF funzione di ripartizione|CDF]] riesce a dare una approssimazione della distribuzione


**Generalized Inverse Function**

Se abbiamo $U \sim Unif(0,1)$ randomiche, e abbiamo $F_X^{-1} (U)$  dobbiamo dimostrare che la $F_X^{-1} (U) \sim X$  

Dobbiamo dimostrare quindi che $Y \sim F^{-1}_x(U)$ e $Y\sim F_X \iff P(X \le x) = F_X(x)$

Proprietà:
1. per l'inversa $F_X(F_X^{-1}(u))= u$
2. if $U \sim Unif(0,1)$ cdf $P(U\le u)= F_U(u)=u$ where $u \in [0,1]$

$P(F^{-1}_X(U) \leq x)=P\left[F_X(F_x^{-1}(U)) \leq F_X(x))\right]=P(U \leq F_{X}(x))=$ utilizzando la proprietà 2  $= F_{X}(X)$

