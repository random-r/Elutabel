lifetable <- function(x, Nx, Dx, data) {
  x <- data[,x]   # x= vanus; 
  Nx <- data[,Nx] # Nx= elavate arv vanuses x;
  Dx <- data[,Dx] # Dx= surmade arv vanuses x 
  
  nmax <- length(x)
  Age <- c(0:(nmax-1))
  mx = Dx/Nx  # suremuskordaja vanuses x
  qx = mx/(1+0.5*mx) # suremustõenäosus vahemikus x ja x+1 q(x)
  qx[nmax] <- 1
  px = 1-qx	 # ellujäämistõenäosus vahemikus x ja x+1 p(x)
  
  lx = dx = Lx = Tx = ex <- rep(0, nmax)
  lx[1] <- 100000
  
  for (i in 2 : nmax) {
    lx[i] <- (lx[i-1] * px[i-1])
    dx[i-1] <- lx[i-1] - lx[i]
    }
  
  dx[nmax] <- lx[nmax]		
  
  for (i in 1: (nmax-1)) {
    Lx[i] <- lx[i+1] + 0.5*dx[i]
    }
  
  Lx[nmax] <- 0.5*dx[nmax]	
  
  Tx <- rev(cumsum(rev(Lx)))
  Tx[nmax] <- 0
  ex = Tx / lx
  
  return(data.frame(
    Age, 
    Nx=round(Nx,1), 
    Dx=round(Dx,1), 
    mx=round(mx,4), 
    qx=round(qx,4), 
    px=round(1-qx,4), 
    lx=round(lx), 
    dx=round(dx,1), 
    Lx=round(Lx,0), 
    Tx=round(Tx,0),
    ex=round(ex,3))
    )
}
