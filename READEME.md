```bash 
dune build #构建项目 
dune exec toyc_compiler #执行main函数 
dune test  #运行测试代码
dune build @lexer
dune build @parser
dune build @typecheck
dune build @codegen  #执行单元测试
```
> 需要测试的代码放在tests 目录下, 运行dune test 没有输出说明测试通过, 否则会报错

