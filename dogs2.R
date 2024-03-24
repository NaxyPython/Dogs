# Chargement des données
Ndogs <- 30
Ntrials <- 25
Y <-
  structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 
              0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
              0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
              0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
              0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 
              0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 
              0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 
              1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 
              1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 
              1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 
              0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 
              0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 
              1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 
              1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 
              0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 
              0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 
              0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
              1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 
              0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 
              1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 
              1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
              0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 
              1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
              1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
              1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 
              1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
              0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
              1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
              1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
              1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 
              1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
              1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
              1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
              1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 
              1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(30, 
                                                                                 25))
#'Y' est la matrice des données binaires 30x25, avec 1 pour le succès et 0 pour l'échec

LogVraisemblance <- function(alpha, beta, Y) {
  Vraisemblance <- 0
  for (i in 1:Ndogs) {
    for (j in 2:Ntrials) {
      xs <- sum(Y[i, 1:(j - 1)])
      logit_p <- alpha * xs + beta * (j - 1 - xs)
      p <- plogis(logit_p) # Transformation logistique pour garder p entre 0 et 1
      Vraisemblance <- Vraisemblance + Y[i, j] * log(p) + (1 - Y[i, j]) * log(1 - p)
    }
  }
  return(Vraisemblance)
}

# Initialisation des paramètres
alpha <- -1
beta <- -1
n_iterations <- 10000
chain_alpha <- numeric(n_iterations)
chain_beta <- numeric(n_iterations)

for (i in 1:n_iterations) {
  # Proposition de nouveaux paramètres
  proposition_alpha <- rnorm(1, mean = alpha, sd = 0.05)
  proposition_beta <- rnorm(1, mean = beta, sd = 0.05)
  
  # Calcul de la probabilité d'acceptation
  log_acceptance_ratio <- LogVraisemblance(proposition_alpha, proposition_beta, Y) - LogVraisemblance(alpha, beta, Y)
  
  # Vérif si log_acceptance_ratio est NaN ou NA
  if (is.nan(log_acceptance_ratio) || is.na(log_acceptance_ratio)) {
    log_acceptance_ratio <- -Inf # pour verif rejet proposition
  }
  
  # Accepter ou rejeter la proposition
  if (log(runif(1)) < log_acceptance_ratio) {
    alpha <- proposition_alpha
    beta <- proposition_beta
  }
  
  # Stockage valeurs des paramètres
  chain_alpha[i] <- alpha
  chain_beta[i] <- beta
}

#densité( valeurs les plus plausibles).

# Densité pour alpha
plot(density(chain_alpha), main="Densité de Alpha", xlab="Alpha", ylab="Densité")
abline(v=mean(chain_alpha), col="red") # Moyenne de alpha

# Densité pour beta
plot(density(chain_beta), main="Densité de Beta", xlab="Beta", ylab="Densité")
abline(v=mean(chain_beta), col="red") # Moyenne de beta

#Chaîne(valeurs des paramètres à chaque itération).
#Chaîne pour alpha
plot(chain_alpha, type="l", main="Chaîne de Alpha", xlab="Itération", ylab="Alpha")
abline(h=mean(chain_alpha), col="red") # Moyenne de alpha

#Chaîne pour beta
plot(chain_beta, type="l", main="Chaîne de Beta", xlab="Itération", ylab="Beta")
abline(h=mean(chain_beta), col="red") # Moyenne de beta


#Autocorrélations
# Pour alpha
acf(chain_alpha, main="Autocorrélation de Alpha")

# Pour beta
acf(chain_beta, main="Autocorrélation de Beta")

# Fonction de log-vraisemblance
Log_Vrai <- function(alpha, beta, Y) {
  LV <- 0
  for (i in 1:Ndogs) {
    for (j in 2:Ntrials) {
      xs <- sum(Y[i, 1:(j - 1)])
      logit_p <- alpha * xs + beta * (j - 1 - xs)
      p <- plogis(logit_p)
      LV <- LV + Y[i, j] * log(p) + (1 - Y[i, j]) * log(1 - p)
    }
  }
  return(LV)
}

# Initialisation des paramètres et stockage des chaînes
alpha <- -1  # Valeur initiale pour alpha
beta <- -1   # Valeur initiale pour beta
n_iterations <- 10000
chain_alpha <- numeric(n_iterations)
chain_beta <- numeric(n_iterations)

# Algorithme de Metropolis-Hastings
set.seed(123)  # Pour la reproductibilité
for (i in 1:n_iterations) {
  # Proposition de nouveaux paramètres à partir d'une distribution normale
  alpha_propose <- rnorm(1, mean = alpha, sd = 0.1)
  beta_propose <- rnorm(1, mean = beta, sd = 0.1)
  
  # Calcul du ratio d'acceptation
  LV_actuel <- Log_Vrai(alpha, beta, Y)
  LV_propose <- Log_Vrai(alpha_propose, beta_propose, Y)
  acceptance_ratio <- exp(LV_propose - LV_actuel)
  
  # Accepter ou rejeter les nouveaux paramètres
  if (runif(1) < acceptance_ratio) {
    alpha <- alpha_propose
    beta <- beta_propose
  }
  
  # Stocker les paramètres de l'itération
  chain_alpha[i] <- alpha
  chain_beta[i] <- beta
}

# Analyse des résultats après l'algorithme
# Moyennes des paramètres
alpha_mean <- mean(chain_alpha)
beta_mean <- mean(chain_beta)
print(paste("Moyenne estimée pour alpha:", alpha_mean))
print(paste("Moyenne estimée pour beta:", beta_mean))
