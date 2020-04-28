PP_Optimizer = function(data, class = NA, findex = "HOLES", dimproj = 2, sphere = TRUE,
                        optmethod = "GTSA", weight = TRUE, lambda = 0.1,  r = 1,
                        cooling = 0.9, eps = 1e-3, maxiter = 3000, half = 30) {
    #browser()

  # Funcao de otimizacao dos indices da projection pursuit, desenvolvida
  # por Paulo Cesar Ossani in 2017/04/06.

  # Entrada:
  # data    - Conjunto de dados numericos sem informacao de classes.
  # class   - Vetor com os nomes das classes dos dados.
  # findex  - Funcao indice de projecao a ser usada:
  #           "lda" - Indice LDA,
  #           "pda" - Indice PDA,
  #           "lr" - Indice Lr,
  #           "holes" - Indice holes (default),
  #           "cm" - Indice massa central,
  #           "pca" - Indice PCA,
  #           "friedmantukey" - Indice Friedman Tukey,
  #           "entropy" - Indice entropia,
  #           "legendre" - Indice Legendre,
  #           "laguerrefourier" - Indice Laguerre Fourier,
  #           "hermite" - Indice Hermite,
  #           "naturalhermite" - Indice Hermite natural,
  #           "kurtosismax" - Indice curtose maxima,
  #           "kurtosismin" - Indice curtose minima,
  #           "moment" - Indice momento,
  #           "chi" - Indice qui-quadrado,
  #           "mf"  - Indice MF.
  # dimproj - Dimensao para a projecao dos dados (default = 2).
  # sphere  - Dados esfericos (default = TRUE).
  # optmethod - Metodo de otmizacao GTSA - Grand Tour Simulated Annealing
  #             ou SA - Simulated Annealing (default = "GTSA").
  # weight  - Usado nos indice LDA, PDA e Lr, para ponderar os calculos
  #           pelo numero de elementos em cada classe (default = TRUE).
  # lambda  - Usado no indice PDA (default = 0.1).
  # r       - Usado no indice Lr(default = 1).
  # cooling - Taxa de arrefecimento (default = 0.9).
  # eps     - Precisao de aproximacao para cooling (default = 1e-3).
  # maxiter - Numero maximo de iteracoes do algoritimo (default = 3000).
  # half    - Numero de etapas sem incremetar o indice, para em seguida
  #           diminuir o valor do cooling (default = 30).

  # Retorna:
  # num.class   - Numero de classes.
  # class.names - Nomes das classes.
  # proj.data   - Dados projetados.
  # vector.opt  - Vetores de projecao encontrados.
  # index       - Vetor com os indices de projecao encontrados no processo.
  # findex      - Funcao indice de projecao usada.

  if (!is.data.frame(data) && !is.matrix(data))
    stop("Entrada 'data' esta incorreta, deve ser do tipo dataframe ou matrix. Verifique!")

  if (!is.na(class[1])) {

    class <- as.matrix(class)

    if (nrow(data) != length(class))
      stop("Entrada 'class' ou 'data' esta incorreta, devem conter o mesmo numero de linhas. Verifique!")
  }

  if (findex %in% c("LDA", "PDA", "LR") && is.na(class[1]))
    stop("Para os indices 'LDA', 'PDA' e 'LR', necessita-se de entrada em 'class'. Verifique!")

  findex <- toupper(findex) # transforma em maiusculo

  if (!(findex %in% c("LDA", "PDA", "LR", "HOLES", "CM", "PCA", "FRIEDMANTUKEY", "ENTROPY",
                      "LEGENDRE",  "LAGUERREFOURIER", "HERMITE", "NATURALHERMITE",
                      "KURTOSISMAX", "KURTOSISMIN", "MOMENT", "CHI", "MF")))
    # stop(paste("Funcao indice:",findex, "nao cadastrada. Verifique!"))
    stop("Entrada para 'findex' esta incorreta, deve ser: 'lda', 'pda', 'lr',
          'holes', 'cm', 'pca', 'friedmantukey', 'entropy', 'legendre',
          'laguerrefourier', 'hermite', 'naturalhermite', 'kurtosismax',
          'kurtosismin', 'moment', 'chi' ou 'mf'. Verifique!")

  if ((findex %in% c("PCA","KURTOSISMAX", "KURTOSISMIN"))  && dimproj != 1)
    stop("Para os indices 'PCA', 'KURTOSISMAX' e 'KURTOSISMIN', 'dimproj' deve ser 1 (um). Verifique!")

  if ((findex %in% c("MOMENT", "CHI", "FRIEDMANTUKEY", "ENTROPY", "LEGENDRE",
                     "LAGUERREFOURIER", "HERMITE", "NATURALHERMITE")) && dimproj != 2)
    stop("Para os indices 'MOMENT', 'CHI', 'FRIEDMANTUKEY', 'ENTROPY', 'LEGENDRE', 'LAGUERREFOURIER', 'HERMITE' e 'NATURALHERMITE', 'dimproj' deve ser 2 (dois). Verifique!")

  if (dimproj >= ncol(data))
    stop("dimproj maior, ou igual ao numero de colunas em data. Verifique!")

  if (!is.logical(sphere))
    stop("Entrada para 'sphere' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (!is.logical(weight))
    stop("Entrada para 'weight' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (!is.numeric(lambda) || lambda < 0 || lambda >= 1 )
    stop("Entrada para 'lambda' esta incorreta, deve ser um valor numerico entre [0,1). Verifique!")

  if (!is.numeric(r) || r <= 0 )
    stop("Entrada para 'r' esta incorreta, deve ser um valor numerico maior que zero. Verifique!")

  optmethod <- toupper(optmethod) # transforma em maiusculo

  if (!(optmethod %in% c("GTSA", "SA")))
    stop("Entrada para 'optmethod' esta incorreta, deve ser do tipo caracter, sendo 'GTSA' ou 'SA'. Verifique!")

  if (nrow(data) < ncol(data) && sphere)
    stop("Para dados esfericos o numero de observacoes deve ser maior, ou igual ao numero de variaveis em data. Verifique!")

  if (!is.numeric(cooling) || cooling <= 0 || cooling >= 1)
    stop("Entrada para 'cooling' esta incorreta, deve ser um valor numerico entre (0,1). Verifique!")

  if (!is.numeric(eps) || eps <= 0 || eps >= 1)
    stop("Entrada para 'eps' esta incorreta, deve ser um valor numerico entre (0,1). Verifique!")

  if (!is.numeric(maxiter) || maxiter < 1 || (floor(maxiter)-maxiter) != 0)
    stop("Entrada para 'maxiter' esta incorreta, deve ser um numero inteiro maior que zero. Verifique!")

  if (!is.numeric(half) || half < 1 || (floor(half)-half) != 0)
    stop("Entrada para 'half' esta incorreta, deve ser um numero inteiro maior que zero. Verifique!")

  set.seed(7) # semente para o processo de aleatoriedade

  #### START - Useful Functions ####
  spheredata <- function(data) {
    # Esfera uma matriz (ou estrutura de dados), ou seja,
    # projecao das variaveis com componentes principais.
    # E um conjunto com a vista guiada, uma vez que remove padroes
    # mais simples que podem ocultar conclusoes mais interessantes.

    apply(predict(prcomp(data)), 2, scale)

    ## Tem o mesmo resultado do anterior
    # Dat <- scale(data, center = T, scale = F) # colunas centralizadas nas respctivas medias
    # a <- var(Dat)
    # t(diag(1/sqrt(eigen(a)$values))%*%t(eigen(a)$vectors)%*%t(Dat)) # projecao dos dados

  }


  Base <- function(NumLin, d) {
    # Esta funcao ajuda a encontrar uma base ortonormal usando as componentes principais

    dataBase <- matrix(rnorm(NumLin * d), ncol = d)

    return(dataBase)
  }


  if (optmethod == "GTSA") { # somente para o metodo de otimizacao grand tour simulated annealing

    Interpolation <- function(Aa, Az) {
      # This function performs matrix Aa interpolation in Az

      # Input:
      # Aa - Initial projection
      # Az - Projection target

      # Return:
      # A - Matrix of interpolated projection of Aa in Az

      if (!is_orthonormal(Aa)) Aa <- Orthonormalise(Aa)

      if (!is_orthonormal(Az)) Az <- Orthonormalise(Az)

      sv <- svd(t(Aa) %*% Az) # decomposicao de valor singular

      # Componentes das decomposicao de varlor singular
      NumCol <- ncol(Aa)
      lambda <- sv$d[NumCol:1] # do menor para o maior lambda para encontrar os planos mais proximos
      Va     <- sv$u[, NumCol:1]
      Vz     <- sv$v[, NumCol:1]

      # Planos para a projecao
      Ba <- Aa %*% Va
      Bz <- Az %*% Vz

      # Ortonormaliza os planos
      Ba <- Orthonormalise(Ba)
      Bz <- Orthonormalise(Bz)
      Bz <- Orthonormalise_by(Bz, Ba)

      # Calcula os angulos principais
      Tau <- suppressWarnings(acos(lambda)) # Gera uma mensagem de aviso que corresponde ao seu argumento
      Tau.NaN <- is.nan(Tau) | Tau < eps
      Tau[Tau.NaN] <- 0

      Bz[, Tau.NaN] <- Ba[, Tau.NaN]

      k <- 1
      # for (k in 1:length(Tau))
      while (k <= length(Tau)) {
        Bz[,k] <- Ba[,k] * cos(Tau[k]) + Bz[,k] * sin(Tau[k])
        k <- k + 1
      }

      A = Bz %*% Va

      return(A)
    }


    Normalise <- function(Base) {
      # Esta funcao normaliza uma Base
      sweep(Base, 2, sqrt(colSums(Base^2,na.rm = TRUE)), FUN = "/")
    }


    Orthonormalise_by <- function(MatX, MatY) {
      # Esta funcao Orthonormalize uma matriz por outra.Isso
      # garante que cada coluna em MatX seja ortogonal a coluna
      # correspondente em por MatY, usando o processo de Gram-Shimidt

      # verifica se as matrizes possuem o mesmo tamanho
      stopifnot(ncol(MatX) == ncol(MatY))
      stopifnot(nrow(MatX) == nrow(MatY))

      MatX <- Normalise(MatX)
      j <- 1
      while(j <= ncol(MatX)) { # processo de ortognalizacao de Gram-Schmidt
        MatX[, j] <- MatX[, j] - c(crossprod(MatX[, j], MatY[, j]) / sum(MatY[, j]^2)) * MatY[, j]
        # MatX[, j] <- MatX[, j] - crossprod(MatX[, j], MatY[, j]) * MatY[, j]
        j <- j + 1
      }

      Normalise(MatX)
    }


    Orthonormalise <- function(Base) {
      # Esta funcao encontra uma base ortogonal ou ortonormal
      # para os vetores em Base usando o processo de Gram-Shimidt

      Base <- Normalise(Base) # to be conservative

      if (ncol(Base) > 1) {
        j <- 1
        while(j <= ncol(Base)) { # processo de ortognalizacao de Gram-Schmidt
          i <- 1
          while(i <= (j - 1)) {
            Base[, j] <- Base[, j] - c(crossprod(Base[, j], Base[, i]) / sum(Base[, i]^2)) * Base[, i]
            #Base[, j] <- Base[, j] - crossprod(Base[, j], Base[, i]) * Base[, i]
            i <- i + 1
          }
          j <- j + 1
        }
      }

      Normalise(Base)
    }


    is_orthonormal <- function(data) {
      # Esta funcao verifica se data e ortonormal

      stopifnot(is.matrix(data))

      Limit <- 0.001

      j <- 1
      while(j <= ncol(data)) {
        if (sqrt(sum(data[, j] ^ 2)) < 1 - Limit) return(FALSE)
        j <- j + 1
      }

      if (ncol(data) > 1) {
        j <- 2
        while(j <= ncol(data)) {
          i <- 1
          while(i <= (ncol(data) - 1)) {
            if (abs(sum(data[, j] * data[, i])) > Limit) return(FALSE)
            i <- i + 1
          }
          j <- j + 1
        }
      }

      TRUE
    }
  }


  if (findex == "CHI") { # funcao usada no indice Qui-quadrado
    # Encontrar a probabilidade de normalizacao bivariada normal sobre cada caixa radial
    fnr <- function(x) { x * exp(-0.5 * x^2) } # veja que aqui a funcao normal padrao bivariada esta em Coordenadas Polares
    ck  <- rep(1,40)
    ck[1:8]   <- integrate(fnr, 0, sqrt(2*log(6))/5)$value/8
    ck[9:16]  <- integrate(fnr, sqrt(2*log(6))/5  , 2*sqrt(2*log(6))/5)$value/8
    ck[17:24] <- integrate(fnr, 2*sqrt(2*log(6))/5, 3*sqrt(2*log(6))/5)$value/8
    ck[25:32] <- integrate(fnr, 3*sqrt(2*log(6))/5, 4*sqrt(2*log(6))/5)$value/8
    ck[33:40] <- integrate(fnr, 4*sqrt(2*log(6))/5, 5*sqrt(2*log(6))/5)$value/8
  } else ck = NA

  #### END - Useful Functions ####

  NumCol <- ncol(data)

  if (sphere) {
    Dat <- spheredata(as.matrix(data)) # projeta dos dados usando as componentes principais
  } else Dat <- as.matrix(data)

  Aa <- diag(1, nrow = NumCol, ncol = dimproj) # Matrix of orthogonal initialization

  if (!(findex %in% c("LDA", "PDA", "LR", "CHI", "MF"))) {

    Proj <- Dat %*% Aa # initial projection

  } else Proj <- Dat

  Proj <- as.matrix(Proj)

  proj.data <- Proj

  indexMax <- PP_Index(data = Proj, class = class, vector.proj = Aa, findex = findex,
                       dimproj = dimproj, weight = weight, lambda = lambda, r = r, ck = ck)$index # Encontra o indice de acordo com findex

  index <- as.matrix(indexMax) # index of intical projection

  mi <- 1
  h  <- 0	# number of iterations without increase in index
  while (mi <= maxiter && cooling > eps) {

    Ai <- Base(NumCol, dimproj) # initial base

    Az <- Aa + cooling * Ai # projecao alvo

    if (optmethod == "GTSA") # somente para o metodo de otimizacao grand tour simulated annealing
      Az <-  Interpolation(Aa, Az) # Matriz de projecao atraves da interpolacao

    if (!(findex %in% c("LDA", "PDA", "CHI"))) {

      Proj <- Dat %*% Az # initial projection

    } else Proj <- Dat

    indexC <- PP_Index(data = Proj, class = class, vector.proj = Az, findex = findex,
                       dimproj = dimproj, weight = weight, lambda = lambda, r = r, ck = ck)  # Encontra o indice de acordo com findex

    indexCurent <- indexC$index

    print(paste("Iteracao <-", round(mi,1),"   Indice <-", round(indexMax,10), "   cooling <-", round(cooling,9)))#,"   i<- ", round(i,1)))

    if (findex == "LDA"   && indexCurent > indexMax ||
        findex == "PDA"   && indexCurent > indexMax ||
        findex == "LR"    && indexCurent > indexMax ||
        findex == "HOLES" && indexCurent > indexMax ||
        findex == "PCA"   && indexCurent > indexMax ||
        findex == "HERMITE"  && indexCurent > indexMax ||
        findex == "ENTROPY"  && indexCurent > indexMax ||
        findex == "LEGENDRE" && indexCurent > indexMax ||
        findex == "LAGUERREFOURIER" && indexCurent > indexMax ||
        findex == "FRIEDMANTUKEY"   && indexCurent > indexMax ||
        findex == "NATURALHERMITE"  && indexCurent > indexMax ||
        findex == "KURTOSISMAX" && indexCurent > indexMax || # favorece detectcao outliers
        findex == "KURTOSISMIN" && indexCurent < indexMax || # favorece detectcao agrupamento
        findex == "MOMENT" && indexCurent < indexMax ||
        findex == "CHI"    && indexCurent < indexMax ||
        findex == "MF"     && indexCurent < indexMax || # menor resultado foi melhor
        findex == "CM"     && indexCurent < indexMax) {

      if ((findex %in% c("LDA", "PDA", "CHI")))
        Proj <- Dat %*% Az # initial projection

      Aa        <- Az
      indexMax  <- indexCurent
      proj.data <- Proj
      index     <- rbind(index, indexCurent)

    } else h <- h + 1

    mi <- mi + 1

    if (h == half) {
      cooling <- cooling * 0.9
      h <- 0
    }

  }

  rownames(index) <- NULL

  rownames(proj.data)  <- rownames(data)

  rownames(Aa) <- colnames(data)

  if (!is.na(class[1])) {

    proj.data <- cbind(as.data.frame(proj.data), class)
    colnames(proj.data) <- c(paste("Projecao", 1:(ncol(proj.data) - 1)),"classes")

  } else colnames(proj.data) <- c(paste("Projecao", 1:(ncol(proj.data))))

  colnames(Aa) <- c(paste("Eixo", 1:(ncol(Aa))))

  if (length(index) > 1) colnames(index) <- "Indices"

  if (!is.na(class[1])) {
    class.Table <- table(class)        # cria tabela com as quantidade dos elementos das classes
    class.names <- names(class.Table)  # nomes das classses
    num.class   <- length(class.Table) # numero de classes
  } else {
    class.names <- NA # nomes das classses
    num.class   <- NA # numero de classes
  }

  Lista <- list(num.class = num.class, class.names = class.names,
                proj.data = proj.data, x = Aa,
                index = index, findex = findex)

  return(Lista)

}
