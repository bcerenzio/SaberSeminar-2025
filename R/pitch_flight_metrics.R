library(tidyverse)

#### calculate release angle & time/velocity vectors at release

vyR_fun <- function(vy0, ay, release_pos_y){
  vy0_sq <- vy0^2
  second_part <- 2*ay*(release_pos_y-50)
  val <- -sqrt(vy0_sq + second_part)
  return(val)
}

tR_fun <- function(vyR, vy0, ay){
  diff <- vyR - vy0
  val <- diff/ay
  return(val)
}

vxR_fun <- function(vx0, ax, tR){
  return(vx0+ax*tR)
}

vzR_fun <- function(vz0, az, tR){
  return(vz0+az*tR)
}

pitch_velo_fun <- function(vxR, vyR, vzR){
  vxR_sq <- vxR^2; vyR_sq <- vyR^2; vzR_sq <- vzR^2
  velo_ftps <- sqrt(vxR_sq + vyR_sq + vzR_sq)
  velo_mph <- velo_ftps*0.6818
  return(velo_mph)
}

release_angle_fun <- function(vxR, vyR, vzR){
  vxR_sq <- vxR^2; vyR_sq <- vyR^2
  denom <- sqrt(sum(vxR_sq, vyR_sq))
  val_radians <- atan(vzR/denom)
  val_degrees <- val_radians*180/pi
  return(val_degrees)
}

release_direction_fun <- function(vxR, vyR){
  val_radians <- atan(-vxR/vyR)
  val_degrees <- val_radians*180/pi
  return(val_degrees)
}

ball_pos_y_fun <- function(release_pos_y, vyR, ay, tR = 0.1){
  first_part <- release_pos_y
  second_part <- vyR*tR
  third_part <- 0.5*ay*tR^2
  final_pos_y <- first_part + second_part + third_part
  return(final_pos_y)
}

ball_pos_x_fun <- function(release_pos_x, vxR, ax, tR = 0.1){
  first_part <- release_pos_x
  second_part <- vxR*tR
  third_part <- 0.5*ax*tR^2
  final_pos_x <- first_part + second_part + third_part
  return(final_pos_x)
}


ball_pos_z_fun <- function(release_pos_z, vzR, az, tR = 0.1){
  first_part <- release_pos_z
  second_part <- vzR*tR
  third_part <- 0.5*az*tR^2
  final_pos_z <- first_part + second_part + third_part
  return(final_pos_z)
}

## calculating spin efficiency

#ball flight function (ie time it takes the pitch to reach 1.4 feet from home plate)

time_flight_fun <- function(vyR, release_pos_y, ay){
  final_pos <- 17/12
  first_part <- vyR
  second_part <- sqrt(vyR^2-2*ay*(release_pos_y-final_pos))
  time_flight <- (-first_part - second_part)/ay
  return(time_flight)
}

avg_velocity_vec_fun <- function(vR, a, tf){
  return((2*vR+a*tf)/2)
}

avg_velocity_tol_fun <- function(vavgx, vavgy, vavgz){
  return(sqrt(vavgx^2 + vavgy^2 + vavgz^2))
}

accel_drag_fun <- function(vavgx, ax, vavgy, ay, vavgz, az){
  vavg <- sqrt(vavgx^2 + vavgy^2 + vavgz^2)
  g <- 32.174
  
  numerator <- vavgx*ax + vavgy*ay + vavgz*(az+g)
  
  return(numerator/vavg)
}


accel_mag_fun <- function(ax, vavgx, adrag, vbar, ay, vybar, az,vzbar){
  amagx <- ax + adrag * vavgx / vavg
  amagy <- ay + adrag * vavgy / vavg
  amagz <- az + adrag * vavgz / vavg
  
  amag <- sqrt(amagx^2 + amagy^2 + amagz^2)
  
  return(amag)
}
  
lift_coef_fun <- function(vavg, amag, K = 5.383e-3){
    vavg_sq <- vavg^2
    
    lift_coef <- amag / (K * vavg_sq)
    
    return(lift_coef)
}

tranverse_spin_fun <- function(vavg, lift_coef){
  
  S <- 0.4*lift_coef/(1-2.32*lift_coef)
  
  spin_t <- 78.92 * S * vavg
  
  return(spin_t)
}

spin_efficiency_fun <- function(spin_t, spin_efficiency){
  return(spin_t/spin_efficiency)
}