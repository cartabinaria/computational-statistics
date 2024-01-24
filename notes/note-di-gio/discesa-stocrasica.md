---
mid: 1694505205907
nid: 1705935973139
tags: 
date: 2024-01-21 22:05
---

## Metodi locali e Globali

**Metodo locale**: si basi sui valori precedentemente calcolati (tende a cercare di minimizzare in base alle soluzioni immediatamente vicinie) di solito in una forma $\theta_{j}=f(\theta_{j-1},\dots,\theta_{0})$

## Deterministic

> di solito sfruttano proprietà delle funzioni (ad esempio convessità, derivabilità etc...)

### Newton-Raphson method
- global method per l'uso delle derivate
- locale per lo spostamento
$\theta_{J+1}=\theta_{J}-[H(\theta)]^{-1}\cdot \nabla h(\theta)$

**Pro**:
- perfetto quando la funzione è quadrata

**Problemi** :
- l'Hessiana da problemi perchè può dare step molto discontinui, e può essere difficile da computare e non sempre è invertibile
- dipende dal punto iniziale quando ha più minimi
- può divergere facilmente se è molto non lineare

### Gradient Method
$\theta_{J+1}=\theta_J+ \alpha_{J}\cdot \nabla h(\theta)$

dove $a_{J}$ è una sequenza decrescente (è una sequenza deterministica)

Per funzioni convesse (e con il dominio in R) riesce a trovare il minimo/massimo, ma normalmente tende a restare nei minimi e massimi locali.

## Stochastic

> nei metodi stocastico di solito la funzione da ottimizzare ha un ruolo meno importante ma deve essere 


## Metodi Stocastici

### stocastico method basilare 

andiamo a generare una sequenza di punti a caso è andiamo a prendere il minimo o il massimo 

Per fare il sample dei dati (ovviamente ci piacerebbe creare i sample dalla stessa distribuzione), ma se non abbiamo informazioni maggiori dobbiamo andare a fare sampling da una uniform

**Tecnica**: possiamo trasformare diventare $h$ positiva $H(\theta)=\exp\left( \frac{h(\theta)}{T} \right)$ (però può ancora divergere) dove T è la temperatura

> per prendere il negativo molt volte possiamo fare $\exp{(-f(x))}$

**Problemi**:
- dipende troppo dal range 
- dipende molto dalla funzione da cui si fa sampling

### Random Walk

, e per andare ad aggiornare il prossimo valore possiamo per esempio $\theta_{j+1}=\theta_{j}+\epsilon_{j}$ dove $\epsilon$ è il componente randomico per esempio **random walk** $\epsilon \sim N(0,\sigma^{2})$

Può essere visto come una [[Def: Catene di Markov a temmpo discreto]]  che ha una serie di proprietà interessanti

> il problema della random walk è che non utilizza nessun elemento del dominio e della valutazione della funzione

### stochastic gradient method

$\nabla h(\theta_{J}) = \frac{h(\theta_{J}+\beta_{J}\cdot \zeta_{J})-h(\theta_{J}-\beta_{J}\cdot\zeta_{J})}{2\beta_{J}}=\nabla \frac{h(\theta_{J};\beta_{J}\cdot\zeta_{J})}{2\beta_{J}}\cdot\zeta_{J}$  

quindi per aggiornare theta $\theta_{J+1}=\theta_{J}+\frac{\alpha_{J}}{2\beta_{J}}\cdot \nabla h(\theta_{J};\beta_{J}\cdot\zeta_{J})\cdot\zeta_{J}$

Dove $\alpha_{J}$ e $\beta_{j}$ sono decresecenti dove $\beta$ è più lenta di $\alpha$ , e dove $\lim_{ j \to \infty }\alpha_{J}/\beta_{J}=0$ 
È definita $\zeta_{J}$  nella unit sphere $\mid\zeta_{J}\mid=1$


### Simulated Annealing


$\theta_{J}=\theta_{J}+\epsilon_{J}$ dove $\epsilon_{J}$ è una sequenza di distribuzioni $\{\pi_{J}\}_{J}$ dove $\pi_{J}$ è proporzionale a $\exp{\frac{h(\theta)}{T_{J}}}$ dove andiamo a modificare la temperatura

> Boltzman-gibbs trasformation è la trasformazione da h a H


Un altro metodo è prendere $\pi_{J}(\theta|x)=L(\theta|x)^{T_{J}}\cdot \pi_{0}(\theta)$ priorior feedback


**Metropolis -hasting step**: servono per andare da $\theta_{i}\to \theta_{i+1}$ by sampling $\pi_{i}(\theta)$
1. si prende un valore da una distribuzione simmetrica $y\sim g(Y)$ 2. $$\theta_{J+1}=\begin{cases} \theta_{j}+y  & \text{con la propbabilità di p}  \\ \theta_{j} & \text{con la propbabilità di 1-p} \end{cases}$$ dove p è definita come $p=\min\left( \exp\left(  \frac{ \nabla h}{T_{i}} \right) ,1\right)$ (questo esponenziale è sempre maggiore di zero se il numeratore è positivo ) dove $\nabla h= h(\theta+y)-h(\theta)$ (quindi se migliori la funzione ti muovi sicuramente se non la migliori non è detto)

la sequenza T di solito la scegliamo tra:
- $T_{i}=\frac{b}{\log(i)}$
- $T_{i}=\alpha^{i}\cdot T_{0}$


> il problema è nella scelta della varianza della funzione da cui si fa sampling