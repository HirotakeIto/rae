# RAEプロジェクト
本レポジトリではRAE論文のソースコードを管理しています。
## フォルダ構成
```
.
├── data: 分析に用いるデータを格納しているフォルダ
├── doc: ドキュメントを格納しているフォルダ
├── renv: renvのconfigフォルダ。renvによる自動生成フォルダで、基本的にここは弄らない。
├── result: 分析結果を格納しているフォルダ
├── src: 分析以外のコードを格納しているフォルダ
│   └── config.yaml: 本レポジトリのconfigファイル。path関連などはここに記述すると良い。
├── task: 分析コード。分析の種類ごとに't\d+(.R|.py)'という名前のファイルに分けている。
├── .Rprofile: renvの生成ファイル。
├── .flake8: pythonのlinterの設定
├── .lintr: Rのlinterの設定
├── .python-version: 利用するpython環境(pyenv)。
├── README.md
├── poetry.lock: poetryが生成する本レポジトリで利用するpythonパッケージとその依存関係を記述したファイル。
├── pyproject.toml: poetryの設定ファイル
├── renv.lock: renvが生成する本レポジトリで利用するRパッケージとその依存関係を記述したファイル。
└── taskrunner.R: Rを用いた分析のタスクランナー。
```
## 利用データについて
伊藤まで確認してください。
アクセス権がある研究者にはデータをお渡ししますので、それをdataフォルダ直下に入れてください。

次の様なデータファイルが存在します。
```
└── data
    ├── dataset1.csv
    ├── dataset1_toda.csv
    ├── saitama_birth.csv
    ├── saitama_birth2.csv
    ├── student_enter.csv
    └── student_first_choice.csv
```

## 論文の図表と対応する分析コード
- doc/tables.md
- doc/figures.md
## 実行環境について
本レポジトリでは次のツールを用いて環境構築を行なっている。
- R: renv
- ptthon: pyenv, poetry
### R
分析コードの多くはRで実行している。
その際の環境構築はrenvで行なっている。
#### Rでの環境構築例
- AWS EC2インスタンスの作成
例えばクラウド上の計算リソースを使うことを考える。ここではAWSのEC2を利用する。
その場合、Rの環境がはじめから用意されているAMI(Amazon Machine Image)として、次のようなものが使える。

https://www.louisaslett.com/RStudio_AMI/

例えば、この中の`ami-0dc8133a777c1be2c`などでセットアップできることを確認している。

クラウド分析環境の利用に慣れてない人は、CyberduckなどのFTPクライアントなどを利用すれば、ファイル(特に元データや分析結果)の取り出しやアップロードをguiを用いて直感的に扱うことができる。


- 環境構築
```
$ git clone https://github.com/HirotakeIto/rae.git
$ cd rae
$ R  # Rのsellに入る
$ install.packages("renv")
$ renv::restore()
```
#### Rでの分析実行例
```
$ cd rae
$ Rscript taskrunner.R
```
### python
#### pythonでの環境構築例

上記のamiを用いて立ち上げた分析環境をそのまま利用するケースを考える。
- pyenvの導入

まずはpyenvを導入する。

https://github.com/pyenv/pyenv

の`Installation` に従って作業(上記amiはubutuイメージなので、その通りに作業を行う)。

```
$ git clone https://github.com/pyenv/pyenv.git ~/.pyenv
$ cd ~/.pyenv && src/configure && make -C src
$ echo 'export PYENV_ROOT="$HOME/.pyenv"' >> ~/.bashrc
$ echo 'export PATH="$PYENV_ROOT/bin:$PATH"' >> ~/.bashrc
$ echo -e 'if command -v pyenv 1>/dev/null 2>&1; then\n  eval "$(pyenv init -)"\nfi' >> ~/.bashrc
$ exec "$SHELL"
```
その後、当プロジェクトに移動して作業をする
```
$ cd ~/rae
$ pyenv install  # .python-versionにあるpythonをインストールする
```
ただし上述環境ではこれだとエラーを吐いて失敗した。
ログを見るとlibffi-devが入っていないことが原因らしい。

```
$ sudo apt-get install libffi-dev
$ pyenv install
```
これでうまくインストールできた。
```
$ python
Python 3.7.1 (default, Jan 29 2021, 07:14:41)
[GCC 7.5.0] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>>
```
と正しくインストールできていることを確認できた。

- poetryの導入
pipを利用してpoetryをインストールすることができる。

```
$ cd ~/rae
$ pip install poetry
$ poetry install  # `poetry.lock`を利用して利用パッケージをインストール
$ poetry shell  # 仮想環境にログイン
```
