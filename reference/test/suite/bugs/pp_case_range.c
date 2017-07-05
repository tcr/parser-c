enum a { a0, a3 };
int error(enum a e)
{
  switch ( e )
  {
  case a0 ... a3:
    return 1;
  }
}