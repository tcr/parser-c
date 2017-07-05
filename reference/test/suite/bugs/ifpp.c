int f(int d)
{
    int i = 0, j, k, l;
    if (d%2==0)
        if (d%3==0)
            i+=2;
        else
            i+=3;
    if (d%2==0)
    {
        if (d%3==0)
            i+=7;
    }
    else
        i+=11;

    l = d;
    if (d%2==0)
        while (l--)
            if (1)
              i+=13;
            else
              i+=17;
    l = d;
    
    if (d%2==0)
    {
        while (l--)
            if (1)
              i+=21;
    }
    else
        i+=23;

    if (d==0)
        i+=27;
    else if (d%2==0)
        if (d%3==0)
            i+=29;
        else if (d%5==0)
            if (d%7==0)
                i+=31;
            else
                i+=33;
    return i;
}
int main()
{
  int i,k=0;
  for(i=0;i<255;i++) 
  {
    k+=f(i);
  }
  printf("Result: %d\n",k);
}