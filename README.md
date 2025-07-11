### About Chroma
Chroma is a powerful vector-based database. It provides collections and official [instruments](https://docs.trychroma.com/docs/collections/manage-collections) to work with them directly in Python or Typescript. Now you can write simple instructions and do not care about embedding generation. Our tool will parse your code and automatically execute all necessary commands for you to make Chroma usage as simple as possible ☺️

### Collections 📚
Chroma Collections are like folders that store *embeddings*, which are special representations of data (like text or images) turned into numerical form so you can search, filter, and analyse them smartly.

### What You Can Do?
You can work with files inside the collection with all possible Chroma instruments and even more 🤩 So you can:
- Add files with given name (content) and optional metadata;
- Get the concrete file content by its ID;
- Update the file content and metadata;
- Search for the nearest files with respect to their content;
- Delete the file;
- Drop the whole collection;
You should use a semicolon `;` at the end of every command

##### ADD
`ADD content of your file here metadata:topic=history,author=Alex;`
1. You **must** specify the command by typing `ADD`
2. You **must** write any content (text) for the file. Embedding will be produced automatically
3. You **may** specify *metadata* for your file. It could be useful while searching. To do so, write `metadata` and then some key-value pairs `key=value` separated by a comma `,`
##### GET
`GET -> doc_123456789;`
1. You **must** specify the command by typing `GET`
2. You **must** use `->` to indicate the following number
3. You **must** specify the file ID as `doc_sequenceOfNumbers`
##### UPDATE
`UPDATE -> doc_123456789 new content of your file here metadata:NewTopic=geography,NewAuthor=Jack;`
1. You **must** specify the command by typing `UPDATE`
2. You **must** use `->` to indicate the following number
3. You **must** specify the file ID as `doc_sequenceOfNumbers`
4. You **must** write any content (text) for the file. Embedding will be produced automatically
5. You **may** specify *metadata* for your file. It could be useful while searching. To do so, write `metadata` and then some key-value pairs `key=value` separated by a comma `,`
##### SEARCH
`SEARCH -> 5 your search string metadata:key1=value1,key2=value2;`
1. You **must** specify the command by typing `SEARCH`
2. You **must** use `->` to indicate the following number
3. You **must** specify the number of expected files
4. You **must** write you search string as a plane text. Chroma will return to you files with the closest content
5. You **may** specify expected *metadata* for the files. To do so, write `metadata` and then some key-value pairs `key=value` separated by a comma `,`
##### DELETE
`DELETE -> doc_123456789;`
1. You **must** specify the command by typing `DELETE`
2. You **must** use `->` to indicate the following number
3. You **must** specify the file ID as `doc_sequenceOfNumbers`
##### DROP
`DROP Y;`
1. You **must** specify the command by typing `DROP`
2. You **must** type `Y` or `y` as an agreement to delete all your files. For any other characters files will not be deleted