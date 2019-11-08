load 'stats/distribs'
load './km.ijs'
load 'graphics/plot'

createCluster =: dyad define 
  size =. x
  'cx cy sx sy' =. y
  xs =. cx + sx * (rnorm size)
  ys =. cy + sy * (rnorm size)
  |: (2,size) $ xs,ys
)

nPerCreateCluster =: 300

NB. create three guassian clusters
c1 =: nPerCreateCluster createCluster 0 3 0.6 0.6
c2 =: nPerCreateCluster createCluster _3 0 0.6 0.6
c3 =: nPerCreateCluster createCluster 3 0 0.6 0.6

NB. combine the cluster into one data set
NB. shuffle just to prove nothing funny is going on
data =: shuffle c1,c2,c3

forPlotting =: (0&{ ; 1&{)@|:


NB. cluster our data
clustering =: 3 km data

pd 'reset'
pd 'type dot'
pd 'color red'
pd forPlotting (I. 0 = clustering) { data

pd 'color green'
pd forPlotting (I. 1 = clustering) { data

pd 'color blue'
pd forPlotting (I. 2 = clustering) { data

pd 'show'

