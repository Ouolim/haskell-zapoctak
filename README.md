# Q+K vs K šachový simulátor

## Uživatelská dokumentace

Tento program simuluje koncovku v šachu: **bílý má krále a dámu proti černému králi**.  
Úkolem programu je, aby bílý (řízený počítačem) sehrál koncovku správně – tedy postupně černého zatlačil a dal mu **mat**, aniž by nastal **pat**.

### Jak spustit
spuštění přes ghci:
```bash
   ghci chess.hs
```
spuštění programu pomocí main
```haskell
    main
```

### Zadání pozic
Program se vás zeptá na počáteční pozice figur. Políčka jsou popisovány jako v šachu - malé písmenko pro sloupec a číslo řádku.
Při stisknutí enteru se použije výchozí pozice

### Průběh hry
Hru začíná bílý, první vypsaná pozice je už po prvním tahu bílého. Poté se čeká, až černý zadá svůj tah. Pokud tah zadáte špatně,
program se zeptá znovu. Tahy černého jsou zadávány stejně jako pří startu hry. Program nedovoluje "sebrání" černého krále.
Pokud počáteční pozice umístí černého krále do šachu, dáma se prvním tahem pohne jinam.

Hra skončí vypsáním výsledku - mat nebo pat. 

Program nekontroluje remízová pravidla - 3x stejná pozice nebo tah, 50 tahů bez tahu pěšcem.

## Programátorská dokumentace

Program je napsán v jazyce Haskell. Herní cyklus je uzavřen do funkce `gameLoop`. Ta vždy:
- vybere tah bílého
- vykreslí šachovnici
- přečte a zvaliduje tah černého
- zavolá sama sebe s novou (nebo starou v případě nevalidního tahu) pozicí
## Datové typy

Stavem programu je trojice políček - pozice bílé dámy, bílého krále a černého krále. Políčko je dvojice  (Int, Int)

## Generování tahů

- Tah krále: všechny sousední pole včetně diagonál (kingMoves).
- Tah dámy: „paprsky“ ve všech 8 směrech, dokud nenarazí na okraj nebo bílého krále (queenRays). Na černého krále by neměla narazit, tedy ho ignoruje.
- Pole ohrožená dámou: podobně jako tahy, ale zastaví se na černém králi (queenAttacks).


## Výběr tahu bílého
Pokud lze dát okamžitý mat, AI vybere tento tah. Jinak se umělá inteligence snaží o klasický mat v rohu - nejdříve je zatlačen černý král do rohu (a střídá dvě políčka),
poté se přiblíží bílý král aby mohl dát bílý mat. Klíčovou funkcí je `blackFreedom`. Ta počítá, na kolik políček se může černý král dostat, kdyby měl neomezeně tahů.
Políčka hlídaná královnou slouží jako bariéra. Tuto hodnotu se snažíme minimalizovat, vybíráme tah královnou, který uzavře krále co nejvíce. 

Pokud je výsledek `blackFreedom = 2`, černý král je v rohu a už není třeba ho omezovat, AI se přiblíží bílým králem. Dává si u toho pozor na pat.

# Testovací data
Program může být vyzkoušen na následujících pozicích:
```haskell

-- černý král v rohu, dáma blízko, testuje matovací tah
gameLoop (7,6) (6,5) (8,8)  -- WK g6, WQ f5, BK h8

gameLoop (4,3) (5,4) (5,6)  -- WK d3, WQ e4, BK e6
gameLoop (5,1) (4,4) (3,8)  -- WK e1, WQ d4, BK c8
gameLoop (5,2) (4,5) (6,6)  -- WK e2, WQ d5, BK f6

```