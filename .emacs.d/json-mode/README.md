json-mode.el
====

Major mode for editing JSON files.

Extends the builtin js-mode to add better syntax highlighting for JSON.

Install
----

```
M-x package-install json-mode
```

You need to have the [marmalade repository](https://marmalade-repo.org/) enabled in emacs for this to work.

Default Keybindings
----

- `C-c C-f`: format the region/buffer with `json-reformat` (<https://github.com/gongo/json-reformat>)
- `C-c C-p`: display a path to the object at point with `json-snatcher` (<https://github.com/Sterlingg/json-snatcher>)

Indent Width
----

See <https://github.com/gongo/json-reformat#configuration> for details on setting the indent width.

License
----

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.
