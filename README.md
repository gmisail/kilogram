# kilogram

Functional programming language inspired by OCaml and Lua.

```lua
record Song
    name: string,
    artist: string,
    album: string,
    length: int,
    num_awards: int
end

record Album
    name: string,
    artist: string,
    songs: list(Song)
end

let new_album = function(album_name: string, artist: string, songs: list(Song)): Album
    Album {
        name: album_name,
        artist: artist,
        songs: songs
    }
end

let is_ep = function(album: Album): bool 
    let runtime = sum(map(function(s: Song): int s.length end, album.songs))
    if runtime <= 30 then true else false
end
```
