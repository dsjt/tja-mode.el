# tja-mode.el

## 概要

tja-mode.elは太鼓さん次郎の譜面ファイルを編集するためのメジャーモードです。

## 機能

* 色分け機能
* BPM計測機能
* 行を指定の拍子数で半角スペース分割


## 導入方法

```` emacs-lisp
(add-to-list 'load-path "tja-mode.elへのパス")
(require 'tja-mode)
````

## 使い方

キーマップは適宜、再設定してください。

### tja-modeにおけるキーバインド
C-c C-l
tja-partition-line
現在行の小節を指定の拍数で分割します。
前置引数で拍数を指定します。(指定しなかった場合は4で分割します。一度指定すると、その値がデフォルトの拍数になります。)

C-c C-h
tja-partition-buffer
バッファ全体の各小節を指定の拍数で分割します。前置引数についてはtja-partition-lineと同様です。

C-c C-q
tja-fill-region
現在選択している範囲の小節の分割を削除します。
範囲が選択されていない場合、エラーになります。

C-c C-j
tja-trace-mode
tja-trace-modeは譜面の作成に特化したマイナーモードです。その他の作業はしにくくなりますが、譜面のトレースが非常に容易にできます。
qでマイナーモードを終了します。


### tja-trace-modeにおけるキーバインド
c
tja-confirm-bpm
BPMの設定を行います。
tja-trace-modeの起動時に譜面でBPMを設定していた場合、その値がデフォルトの設定になります。

j,f
tja-trace-dong
k,d
tja-trace-ka
譜面を作成を開始します。
BPMが指定されていない場合、エラーになります。
入力の仕方は太鼓さん次郎のデフォルトの設定と同じです。
デフォルトで16分まで入力可能です。
BPMに照らし、入力がないタイミングでは、0が挿入されます。
1小節毎に入力が反映されます。
qで譜面の作成を中断します。この時に限り、tja-trace-modeは終了しません。

y
tja-bpm-count
BPMの計測を行います。
拍子ごとにyを押すことにより、BPMを計算してミニバッファに表示します。
