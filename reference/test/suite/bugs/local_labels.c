void foo() {
  do {
    __label__ go;
    {
      __label__ foo,bar;
      foo: 
      bar: ;
    }
    go: ; 
    foo: ;    
  } while(0);
}
