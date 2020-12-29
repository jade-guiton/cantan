# cantan

Yet another toy language VM (yes, that's the third)

This started as an experiment to rewrite Hissy (my previous VM) using the gc_arena crate, but ended up becoming its replacement (and I eventually dropped the use of gc_arena anyway). It is not quite as optimized as Hissy, but the code is a bit simpler for it.

Unlike Hissy, Cantan does not have any static type inference/checking, and it also uses a hybrid stack/register VM, similar to Lua.

## CLI usage

```
Usage:
  cantan parse <src>
  cantan compile <src>
  cantan run <src>
  cantan repl [-d]
  cantan --help | -h

Arguments:
  <src>        Path to a Cantan source file (usually .cn)

Options:
  --help, -h   Print this help message
  -d           Show run bytecode
```

## Examples

**Fizzbuzz:**
```
for(n in range(100))
	let res = ""
	if(n % 3 == 0)
		res += "Fizz"
	end
	if(n % 5 == 0)
		res += "Buzz"
	end
	if(res == "")
		res += repr(n)
	end
	writeln(res)
end
```

**Quicksort:**
```
let quicksort(l)
	if(l.size() <= 1) ret l end
	
	let pivot = l[l.size() // 2]
	
	let smaller = []
	let equal = []
	let greater = []
	for(x in l)
		if(x < pivot)
			smaller.push(x)
		elseif(x > pivot)
			greater.push(x)
		else
			equal.push(x)
		end
	end
	
	ret quicksort(smaller) + equal + quicksort(greater)
end

let l = [5, 7, 8, 10, 1, 2]
log(quicksort(l))
```

**Struct demo:**
```
let Person(name, age)
	let self = {
		name = name,
		age = age,
		talk()
			writeln("What up, I'm " + self.name + ", I'm " + repr(self.age))
		end,
		grow_up()
			self.age = self.age + 1
		end,
	}
	return self
end

let me = Person("Jared", 19)
me.talk()
me.grow_up()
me.talk()
```
