paste('实践8(字符型向量)')
a=factor(letters[1:10])
a[3]='w'
a=as.character(a)
a[3]='w'
a
factor(a)
