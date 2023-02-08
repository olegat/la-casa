#!/usr/bin/env python3

import collections.abc
import enum
import random
import sys

class Pos(enum.Flag):
  START = enum.auto()
  MID   = enum.auto()
  END   = enum.auto()


class Kind(enum.Enum):
  VOWEL     = enum.auto()
  CONSONANT = enum.auto()


class Sym:
  def __init__(self, text:str, kind:Kind, position:Pos):
    self.text     = text
    self.kind     = kind
    self.position = position

  def __repr__(self):
    return f'{{"{self.text}", {self.kind}, {self.position}}}'


class SymList:
  def __init__(self, syms=None):
    if syms == None:
      syms = default_symbolist()
    self._iter = syms;

  def _maybe_expand(self):
    if isinstance(self._iter, filter):
      self._iter = list(self._iter)

  def __repr__(self):
    return str(self._iter)

  def __len__(self):
    self._maybe_expand()
    return len(self._iter)

  def __getitem__(self, key):
    self._maybe_expand()
    return self._iter.__getitem__(key)

  def __setitem__(self, key, value):
    self._maybe_expand()
    return self._iter.__setitem__(key, value)

  def filter(self, kind=None, position=None):

    def maybe_in( value, maybe_iter ):
      if isinstance( maybe_iter, collections.abc.Iterable ):
        return value in maybe_iter;
      return False

    def fn(sym):
      return \
        (kind == None or sym.kind == kind or maybe_in(sym.kind, kind)) and \
        (position == None or (sym.position & position) == position)

    return SymList(filter( fn, self._iter ))

  def randelem(self):
    i = random.randint(0, len(self)-1)
    return self._iter[i]


class WordGenerator:
  def __init__(self, symlist:SymList=None, minm=1, maxm=2):
    if symlist == None:
      symlist = SymList()
    self.symlist = symlist
    self.minm = minm
    self.maxm = maxm
    # Precompute filtered lists:
    self._mids = [None, None]
    self._ends = [None, None]
    self._starts = symlist.filter(position=Pos.START)
    self._mids[0] = symlist.filter(kind=Kind.VOWEL,     position=Pos.MID)
    self._mids[1] = symlist.filter(kind=Kind.CONSONANT, position=Pos.MID)
    self._ends[0] = symlist.filter(kind=Kind.VOWEL,     position=Pos.END)
    self._ends[1] = symlist.filter(kind=Kind.CONSONANT, position=Pos.END)

  def gen_symbols_sequence(self):
    result = [self._starts.randelem()]
    i = 0 if result[0].kind == Kind.CONSONANT else 1
    for _ in range( random.randint(self.minm, self.maxm) ):
      result.append( self._mids[i].randelem() )
      i = (i + 1) % 2
    result.append(self._ends[i].randelem())
    return result


# Many backend require at least one number and one capital letter.
# This special generator generates exactly of each.
class TagGenerator:
  def __init__(self):

    def _syms(texts:list, kind:Kind):
      return [Sym(str(text), kind, Pos.START|Pos.END) for text in texts]

    # Exclude [1, I] and [0, O] which are easily confused.
    nums  = _syms([2, 3, 4, 5, 6, 7, 8, 9], Kind.VOWEL)
    chars = _syms(list('ABCDEFGHJKLMNPQRSTUVWXYZ'), Kind.CONSONANT)
    self._wg = WordGenerator(SymList(nums+chars), minm=0, maxm=0)

  def gen_symbols_sequence(self):
    return self._wg.gen_symbols_sequence();


class PasswordGenerator:
  def __init__(self, symlist:SymList=None):
    self._wg = WordGenerator(symlist)
    self._tg = TagGenerator();

  def gen_password(self):
    gs = [self._wg] * 3
    gs.insert( random.randint(0,2), self._tg )
    words = [''.join([s.text for s in g.gen_symbols_sequence()]) for g in gs]
    return '-'.join(words)


def default_symbolist():
  vowl, cons = (Kind.VOWEL, Kind.CONSONANT)
  start, mid, end = (Pos.START, Pos.MID, Pos.END)
  anywhere = start | mid | end
  return [
    Sym(  'a', vowl, start | mid | end),
    Sym(  'e', vowl, start | mid | end),
    Sym(  'i', vowl, start | mid | end),
    Sym(  'o', vowl, start | mid | end),
    Sym(  'u', vowl, start | mid | end),
    Sym(  'y', vowl, mid | end),
    Sym( 'aa', vowl, mid),
    Sym( 'ae', vowl, mid),
    Sym( 'ai', vowl, start | mid | end),
    Sym( 'ao', vowl, mid),
    Sym( 'au', vowl, start | mid),
    Sym( 'ay', vowl, mid | end),
    Sym( 'ea', vowl, mid | end),
    Sym( 'ee', vowl, mid | end),
    Sym( 'ei', vowl, start | mid | end),
    Sym( 'eo', vowl, mid),
    Sym( 'eu', vowl, mid),
    Sym( 'ey', vowl, mid | end),
    Sym( 'er', vowl, end),
    Sym('eau', vowl, end),
    Sym('eei', vowl, mid),
    Sym( 'ia', vowl, mid | end),
    Sym( 'ie', vowl, mid | end),
    #Sym( 'ii', vowl, mid),
    Sym( 'io', vowl, mid | end),
    Sym( 'iu', vowl, mid),
    Sym( 'oa', vowl, mid),
    Sym( 'oe', vowl, mid),
    Sym( 'oi', vowl, mid),
    Sym( 'oo', vowl, mid),
    Sym( 'ou', vowl, mid | end),
    Sym('ooi', vowl, mid),
    Sym('oui', vowl, mid),
    Sym('oei', vowl, mid),
    Sym( 'ua', vowl, mid),
    Sym( 'ue', vowl, mid),
    Sym( 'ui', vowl, mid),
    Sym( 'uo', vowl, mid),
    Sym( 'uu', vowl, mid),
    Sym(  "'", cons, mid), # glottal stop
    Sym(  'b', cons, start | mid | end),
    Sym(  'c', cons, start | mid | end),
    Sym(  'd', cons, start | mid | end),
    Sym(  'f', cons, start | mid | end),
    Sym(  'g', cons, start | mid | end),
    Sym(  'h', cons, start | mid | end),
    Sym(  'j', cons, start | mid),
    Sym(  'k', cons, start | mid | end),
    Sym(  'l', cons, start | mid | end),
    Sym(  'm', cons, start | mid | end),
    Sym(  'n', cons, start | mid | end),
    Sym(  'p', cons, start | mid | end),
    Sym(  'q', cons, start | mid | end),
    Sym(  'r', cons, start | mid | end),
    Sym(  's', cons, start | mid | end),
    Sym(  't', cons, start | mid | end),
    Sym(  'v', cons, start | mid | end),
    Sym(  'w', cons, start | mid | end),
    Sym(  'x', cons, start | mid | end),
    Sym(  'y', cons, start | mid | end),
    Sym(  'z', cons, start | mid | end),
    Sym( 'bl', cons, start | mid),
    Sym( 'br', cons, start | mid),
    Sym( 'ch', cons, start | mid | end),
    Sym( 'ck', cons, end),
    Sym( 'cl', cons, start | mid),
    Sym( 'cr', cons, start | mid),
    Sym( 'dr', cons, start | mid),
    Sym( 'fl', cons, start | mid),
    Sym( 'fr', cons, start | mid),
    Sym( 'fs', cons, end),
    Sym( 'gh', cons, start | mid | end),
    Sym( 'gl', cons, start | mid),
    Sym( 'gl', cons, start | mid),
    Sym( 'gn', cons, start | mid),
    Sym( 'gr', cons, start | mid),
    Sym( 'gs', cons, end),
    Sym('ght', cons, end),
    Sym( 'kh', cons, start | mid | end),
    Sym( 'kk', cons, mid | end),
    Sym( 'kl', cons, start | mid),
    Sym( 'kl', cons, start | mid),
    Sym( 'kr', cons, start | mid),
    Sym( 'ks', cons, end),
    Sym( 'kw', cons, start | mid),
    Sym( 'ld', cons, mid | end),
    Sym( 'lk', cons, mid | end),
    Sym( 'll', cons, start | mid | end),
    Sym( 'lm', cons, end),
    Sym( 'ln', cons, end),
    Sym( 'mm', cons, mid | end),
    Sym( 'nd', cons, end),
    Sym( 'ng', cons, start | mid | end),
    Sym( 'nk', cons, end),
    Sym( 'nt', cons, end),
    Sym( 'ph', cons, start | mid | end),
    Sym( 'pl', cons, start | mid),
    Sym( 'pp', cons, mid | end),
    Sym( 'pr', cons, start | mid),
    Sym( 'pt', cons, start | mid | end),
    Sym( 'qu', cons, start | mid | end),
    Sym( 'rr', cons, mid | end),
    Sym( 'rh', cons, start | mid),
    Sym( 'sh', cons, start | mid | end),
    Sym( 'sk', cons, start | mid | end),
    Sym('sch', cons, start | mid | end),
    Sym( 'th', cons, start | mid | end),
    Sym( 'tr', cons, start | mid),
    Sym( 'tw', cons, start),
    Sym( 'tt', cons, mid | end),
    Sym('thr', cons, start | mid),
    Sym( 'wk', cons, end),
    Sym( 'wl', cons, end),
    Sym( 'wm', cons, end),
    Sym( 'wn', cons, end),
    Sym('wgn', cons, end),
    Sym( 'wt', cons, end),
    Sym('wth', cons, end),
    Sym( 'zz', cons, mid),
  ]

def main(argv):
  pwg = PasswordGenerator()
  for _ in range(100):
    print(pwg.gen_password())
  # wg = WordGenerator()
  # for _ in range(100):
  #   slist = [x.text for x in wg.gen_symbols_sequence()]
  #   print( f'{"".join(slist)}  ({", ".join(slist)})')
  return 0

if __name__ == '__main__':
  exit(main(sys.argv))
