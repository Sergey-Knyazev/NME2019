# DK = no
my_net<-function(duration, stat, n) {
nw <- network.initialize(100, directed = FALSE)
coef.diss <- dissolution_coefs(~offset(edges), duration)
formation <- ~edges + degree(0:2) + degrange(from = 4)
netest(nw, formation, stat, coef.diss)
}
my_sim<-function(est, param, nsim, nsteps) {
r<-list()
r$dx <- netdx(est, nsims = 5, nsteps = nsteps,
            nwstats.formula = ~edges + degree(0:4))
init <- init.net(i.num = 1)
control <- control.net(type = "SIS", nsims = nsim, nsteps = nsteps)
r$sim <- netsim(est, param, init, control)
return(r)
}

#a<-my_net(22,c(45.5, 35, 45, 14, 0), 100)
b<-my_sim(a, param.net(inf.prob = 0.4, act.rate = 2, rec.rate = 0.01), 1, 500)

nw <- get_network(b$sim)

nw <- color_tea(nw, verbose = FALSE)

slice.par <- list(start = 1, end = 500, interval = 1,
                  aggregate.dur = 1, rule = "any")
render.par <- list(tween.frames = 10, show.time = FALSE)
plot.par <- list(mar = c(0, 0, 0, 0))

compute.animation(nw, slice.par = slice.par, verbose = TRUE)

render.d3movie(
  nw,
  render.par = render.par,
  plot.par = plot.par,
  vertex.col = "ndtvcol",
  edge.col = "darkgrey",
  vertex.border = "lightgrey",
  displaylabels = FALSE,
  filename = paste0(getwd(), "/movie.html"))