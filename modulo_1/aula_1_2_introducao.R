#1.1 Operações aritméticas simples
1+2; 2-1; 1*2; 4/2
#1.2 Variáveis (objetos)
a<-10
b<-20

#Visualizar valores
print(a); print(b)

#1.3 Case-senstivie
d<-35
D<-55
print(d); print(D)

#2.1 Tipos de dados
numero_int<-18
numero_real<-18.5
texto_string<-'SUS'
bool_logico<-TRUE

#Retorna o tipo
class(numero_int); class(numero_real); class(texto_string); class(bool_logico)

#2.2 Convertendo tipos
e<-15.6; print(e); class(e)
e_int<-as.integer(e_int);class(e_int)

#Converte string para numérico
f<-"25"; class(f) 
as.numeric(f)
class(f)

#2.3 Outros tipos
#Vetores
idades<-c(25,30,45,52,68); print(idades)

#Fatores
sexo<-factor(c("M","F","F","M")); print(sexo)

#Matrizes
matriz<-matrix(c(1,2,3,4,5,6),nrow=2,ncol=3); print(matriz)

#3.1 Funções
sum(1,3)
sum(10,20,30)
sqrt(16)
is.numeric(10) #Valida se é numérico
is.numeric('texto')