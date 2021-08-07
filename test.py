from typing import Iterable
from itertools import count, islice


# def zPlus() -> Iterator[int]:
#     i = 2
#     while True:
#         yield i
#         i += 1


# def removeFactor(f: int, ls: Iterable[int]) -> Iterator[int]:
#     return filter(lambda x: x % f > 0, ls)


# def sieve():
#     ls = count(2)
#     while True:
#         e = next(ls)
#         ls = removeFactor(e, ls)
#         yield e


# def take(n: int, ls: Iterator[int]) -> Iterator[int]:
#     for _ in range(n):
#         yield next(ls)

# def sieve():
#     ls = count(2)
#     while True:
#         e = next(ls)
#         ls = filter(lambda x: x % e > 0, ls)
#         yield e


def sieve2(it: Iterable[int]):
    e = next(it)
    yield e
    yield from sieve2(x for x in it if x % e > 0)

sieve = sieve2(count(2))


for e in islice(sieve, 10):
    print(e)
