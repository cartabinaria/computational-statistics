---
mid: 1694505205907
nid: 1705856584474
tags: 
date: 2024-01-07 12:28
---
> Algoritmo ad alto livello, generiamo dei valori a caso e guardiamo se soddisfano i nostri criteri, per capire quel'è la CDF della funzione (in casi come la distribuzione gaussiana non c'è una formula chiusa per risolvere l'integrale della CDF)

Dobbiamo avere una densità $f$ che vogliamo approssimare, e un altra distribuzione $g$ da cui generiamo candidati (g dovrebbe essere più facile da  estrarre esempi)

Requirements:
- compute $f_{X}(x)$
-  $f$ e $g$ hanno compatibile supporto $g(x)>0 \implies f(x)>0$  
- Esiste $M$ t.c. $\frac{f(X)}{g(x)} \le M$  per ogni $x$  

$X \sim f$ può essere simulata come:
- prendiamo un valore $Y \sim g$  (Y da g generato a caso) inoltre prendiamo un altro valore generato a caso $U \sim U[1,0]$
- generiamo $U \le \frac{1}{M} \frac{f(Y)}{g(Y)}$ allora $X=Y$  (Y è un valore di X)



**Proof**:

$$P\left(  Y\le x| U \le \frac{1}{M} \frac{f(Y)}{g(Y)} \right) = \frac{P\left(  Y \le x, U\le \frac{1}{M} \frac{f(Y)}{g(Y)} \right)}{P\left( U\le \frac{1}{M} \frac{f(Y)}{g(Y)} \right)}$$
$$\frac{\int_{-\infty}^{x} \int_{0}^{\frac{1}{M} \frac{f(Y)}{g(Y)}} 1 \, \space du  g(y) \,   dy }{\int_{0}^{\frac{1}{M} \frac{f(Y)}{g(Y)}} 1 \, \space du }$$

Per rimuovere la Y nel dominatore la Integriamo 
$\int_{-\infty}^{\infty} \int_{0}^{\frac{1}{M} \frac{f(y)}{g(y)}} 1 \, \space du  g(y) \, dy$

quindi continuiamo 
$$\frac{\int_{-\infty}^{x} \frac{1}{M} \frac{f(Y)}{g(Y)} g(y) \,   dy }{\int_{-\infty}^{\infty}{\frac{1}{M} \frac{f(y)}{g(y)}} g(y) \, \space du } = \frac{{\int_{-\infty}^{x} f(y) \, dy}}{\int_{-\infty}^{\infty} f(y) \, dy } = \int_{-\infty}^{x} f(y) \, dy = F_{X}(x) $$

**Come scegliere $g$ e $M$**:

>Qualcosa di più su $M$, $M= \sup_x \frac{f(x)}{g(x)}$, inoltre M è l'expecting waiting time (cioè se M = 5 circa $1/5$ funzionerà), ci sarebbe molto da dire ma noi utilizzeremo un metodo per prendere l'M minore possibile

**Proof**:
dobbiamo dimostrare  $P(\text{accepting})=\frac{1}{M}$ se abbiamo $\frac{f}{g} \le M$
$P(\text{acceptiong})=P\left( U \le \frac{1}{M} \frac{f(y)}{g(y)}\right)=P\left( U \le \frac{1}{M} \frac{f(y)}{g(y)}|Y=y \right)=$
$$\int_{-\infty}^{\infty} \int_{-\infty}^{\frac{1}{M} \frac{f(y)}{g(y)}} 1 \, \space du  g(y) \, dy = \int_{-\infty}^{\infty}{\frac{1}{M} \frac{f(y)}{g(y)}} g(y) \, \space du= \int_{-\infty}^{\infty} \frac{1}{M} f(y) \, dy=\frac{1}{M}$$


sfortunatamente la migliore funzione $g$ è $f$ , comunque $g$ dovrebbe essere il più vicino possibile a f anche se scalata in maniera diversa (questo permette di creare un algoritmo più preciso)

mentre M lo andremo a scegliere analiticamente, ma numericamente

## Unormalized density

> questo algoritmo fa anche una stima della constante di normalizzazione (per esempio nella gaussiana è $\frac{1}{\sqrt{ 2 \pi }}$)

si può utilizzare anche con le Unormalized density, essendo che le constanti vengono assoribite da M

Per calcolare il valore della costante di normalizzazione basta:

![[Pasted image 20240121155657.png]]

> **exam tips**: il valore della normalizzazione è uguale al valre della integrale  

