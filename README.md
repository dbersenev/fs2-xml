# fs2-xml
**Scala FS2 XML extensions**

Provides streaming parsing for XML using FS2.

There are several components

**EventStream**

Simply produces stream of XML events from some Stream\[?, Byte\].

**SelectedEventStream**

Filters XML event streams. Selects portions of that.
May stop producing events based on some selector.
Main idea here is selector.
Examples:

```
<root>
<unit seq = "1">
<items>
<item a="x">
value 1
</item>
<item a="y">
value 2
</item>
</items>
</unit>
<unit seq = "2">
<items>
<item a = "z">
value 3
</item>
</items>
</unit>
```

```
root("root") |\!| "unit" |\| "items" |\| "item"
produces all "item" events from unit(@seq=1)

root("root") |\| "unit" |\| "items" |\| "item"
produces all "item" events from all "unit"

root("root") |\| "unit" |\| "items" |\!| "item"
produces 2 "item" events: from unit(@seq=1) and from unit(@seq=2)

root("root") |\| ("unit", XMLSelectorAttr("seq", "1".some))
produces events from unit(@seq=1)
```
Options can be used to tune behavior.
ExcludeLastSelectorElement - removes last selector element from the events output.
StopBeforeSelector - stops further processing of elements at soem selector.


**ElementStream**

Folds event stream to stream of elements.

**ElementConversion**

Allows one to convert element into some model.
Implemented as extensions on Nodes and simple ElementConversion typeclass.



