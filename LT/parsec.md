Parsec 入門 
===========


Recursive Decent Parsing - 再帰下降構文解析
-------------------------------------------

再帰下降構文解析とは...

再帰的な構文を上から舐めて行くパーサです。

以下のようなulとolとliの三つのタグからなるhtmlがあるとします

`ブラウザでの表示例`

<div style="border-style:dashed">
<ul>
  <li> Language
    <ul>
      <li> English </li>
      <li> Japanese </li>
    </ul>
  </li>
  <li> Locale
    <ol>
      <li> First </li>
      <li> Second </li>
      <li> Third </li>
    </ol>
  </li>
</ul>
</div>

`html`

```html
<ul contenteditable=true>
  <li> Language
    <ul>
      <li> English </li>
      <li> Japanese </li>
    </ul>
  </li>
  <li> Locale
    <ol>
      <li> First </li>
      <li> Second </li>
      <li> Third </li>
    </ol>
  </li>
</ul>
```

上のような文は以下のような再帰的な構文を持つものとしてパースすることができます。

`EBNF記法`

```
listheader = "<ul>", listbody, "</ul>" | "<ol>", listbody, "</ol>" ;
listbody = { "<li>", listcontents, "</li>" } ;
listcontents = rawstring | listheader
```
上ではstringの定義を抜いていますが
ここではアルファベットからなる文字列とします。

またホワイトスペースのパース記述も省略しています。


haskellのように関数がファーストクラスな言語は
再帰下降構文解析は非常に相性がいいです。

パーサコンビネータ パース成功時に



