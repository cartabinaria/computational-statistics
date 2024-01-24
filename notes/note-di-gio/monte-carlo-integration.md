---
mid: 1694505205907
nid: 1705856584299
tags: 
date: 2024-01-07 12:28
---

> monte carlo integration  è una tecnica socastica, per un problema deterministico, il problema è che la soluzione numerical non funziona in tutti i casi


**assumption** f(x) è una densità potenziale
**possiamo risolvere con questo metodo** $\int_{X} h(x) f(x) \, dx$


la prima uguaglianza l'abbiamo [[Def:Media o valore atteso ( di variabili aleatorie continue) e teoremi]] la seconda possiamo semplicemente riscriverlo vedendo come h(x) una variabile $X$ è [[Def:Variabile aleatoria continua]] 
$\int_{X} h(x)f(x) \, dx=E_{f}[h(x)]=E_{f}[X_{i}]=\mu$ 
 da qua possiamo applicare quindi il [[Legge dei Grandi numeri]]
$\hat{h}_{n}= \frac{1}{n}\sum^{n}_{i=1}h(x_{i})=\mu$ con $n \rightarrow \infty$

> quindi facendo sampling da f possiamo e calcolandoci la media sui punti generati dal sampling per h possiamo risolvere l'integrale

Ci interessa anche capire il possibile errore per cui ci andiamo a calcolare la varianza (è una cosa in più che ci possiamo calcolare):[[Central Limit Theorem]]
$Var(h(x)_{m})=\frac{\sigma^{2}}{n}$  quindi possiamo calcolarci $\sigma^{2}=Var[h(x)]=E_{f}[(h(x)-E_{f}(h(x)))^2]= \int_{X} (h(x)-E_{f}(h(x)))^2 f(x)\, dx$ dove abbiamo già detto che la media per n che si avvicina all'infinito tende a $\hat{h_{n}}$ quindi $\int_{X}  (h(x)-\hat{h_{n}})f(x)\, dx$ quindi possiamo risolverlo con la stessa formula di prima dicendco che $h(x)-\hat{h_{n}}$ è la nuova $h(x)$ quindi abbiamo un approssimazione di $\sigma^{2} = \frac{1}{n} \sum^{n}_{i=1}(h(x_{i})-\hat{h_{n}})^{2}$

per avere i confidence interval $\pm 2 \cdot \sqrt{ \frac{\sigma^{2}}{n}}$


> il central [[Central Limit Theorem]] gli servono più valori per essere affidabile rispetto alla [[Legge dei Grandi numeri]] quindi la media converge prima (e meglio) della varianza

## Esempio carino

Proviamo a simulare $F_{X}(k) \sim N(0,1)$ quindi abbiamo, $\int_{\chi} \mathbb{1}_{x<k} \frac{1}{\sqrt{ 2\pi }} e^{-x^{w}/2} \, dx$ , (per calcolare la media basta simulare da una normale e contare i valori minori di k) 
Possiamo però calcolare quanto è il minimo che serve, infatti possiamo trasformarla in una bernulli che ha come varianza $p(1-p)$ quindi la varianza è $\frac{1}{4}n$ che in deviazione standard è $\sqrt{ \frac{1}{4}n }$ cioè margin of error $\pm 2\cdot$ dev standard

## Important sampling 

Tecnica utilizzata in quanto per esempio è difficile fare sampling da f

Riscriviamo il problema come:
$E_{f}[h(x)] =\int_{X} h(x)\frac{f(x)}{g(x)}g(x)  \, dx=E_{g}\left[ h(x) \frac{f(x)}{g(x)} \right]$

Trovare una buona distribuzione $g$:
- $\frac{f(x)}{g(x)}<M, \forall x \in X$  $var_{f}[h(x)]<-\infty$
- $X$ is compact and $g(x)>0 ,\forall x\in X$ 
- $dom(g) \subset dom(f)\times dom(h)$


<!-- vecchi appunti da sistemare

$Var(X) = E[(X- \mu)^2]= \int_{-\infty}^{\infty} (x-\mu)^2 f_{X}(x) \, dx$ 
$E[h(x)]=\bar{h}(x)=\sum^n_{i=1} \frac{h(x_{i})}{n}= \mu_{h(x)}$

>si può sempre utilizzare la funzione uniforme
> $\int _{X} h(x) f(x)  \, dx =$  essendo che f(x) è una densità uniforme
> $(b-a)\int _{X} h(x) \cdot \frac{1}{b-a}  \, dx$ se $X=[a,b]$ 
> il problema della uniforme è che si da la stessa importanza a tutti i punti di integrazione


**se assumiamo anche che** $E_{f}(h^2(x))<\infty$ allora possiamo dire $\frac{{\bar{h}_{n}-E_{f}[h(X)]}}{\sqrt{ var(\bar{h}_{n}) }} \to N(0,1)$

$Var[h(x)]=E_{f}[(h(x)-E[h(x)])^2]= \sum_{i=1}^n \frac{h'(x_{i})}{n}$ 
$\int_{X}  (h(x)-\bar{h}_{n})^2 \cdot f_{X}(x) \, dx= \int_{X} h'(x) \cdot f_{x} \, dx$
-->