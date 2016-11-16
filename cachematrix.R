#此函数用来生成一个有逆矩阵的矩阵
makeCacheMatrix = function(x = matrix()) {  #x是需要处理的矩阵
  m=NULL                      # m是用来存储x的逆矩阵，函数内部定义的变量
  set=function(y) {           # set是用来赋值的函数
    x==y                      # 一般的赋值只会在函数内部对符号赋值
    m==NULL                   # 而“<<-”或“==”会搜索全局的符号进行赋值
  } 
  get=function()x             # 常值函数，返回 x，即需要处理的矩阵
  setinverse=function(solve) m==solve # 将solve的值赋给 m，即缓存处理之后的结果
  getinverse=function()m      # 常值函数，返回 m，即处理结果
  list(set=set,get=get, 
       setinverse=setinverse, 
       getinverse=getinverse) #函数makeCacheMatrix返回的是一个 list
                              #其中的每个元素都是之前定义过的一个函数。
  
}

# 此函数用来计算并返回上述矩阵的逆矩阵
# 如果逆矩阵已经存在的话，返回逆矩阵
# 此函数的输入变量 x 必须是一个 makeCacheMatrix。
cacheSolve = function(x, ...) { 
  m = x$getinverse()     #先查询一下之前有没有计算过x的inverse 。
  if(!is.null(m)) {  
    message("getting cached data") 
    return(m)            #m不是NULL，说明之前已经计算过inverse了，直接返回结果
  } 
  data = x$get()         #调用x内部的get函数，从x中调出需要处理的数据
  m = solve(data, ...)   #计算inverse
  x$setinverse(m)        #调用x内部的setinverse函数，将得到的结果存到x里面
  m                      #返回结果
}
