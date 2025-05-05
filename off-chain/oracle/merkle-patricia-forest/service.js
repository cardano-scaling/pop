import http from 'http';
import { Store, Trie } from '@aiken-lang/merkle-patricia-forestry';
import express from 'express';
import fs from 'fs';

const db_dir = process.env.DB_DIR || './db';

// report directory exists
if (fs.existsSync(db_dir)) {
    console.log('db directory exists');
}
else {
    console.log('db directory does not exist');
}

const error = (code, message, err, res) => {
    res.writeHead(code, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify({ error: message, details: err ? err.message : undefined }));
}

const success = (code, json, res) => {
    res.writeHead(code, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify(json));
}

const handleInsert = (trie) => async (req, res) => {
    try {
        const { key, value } = req.body;
        if (!key || !value) {
            return error(400, "Key and value are required", res);
        }
        if (typeof key !== 'string' || typeof value !== 'string') {
            return error(400, "Key and value must be strings", res);
        }
        const oldValue = await trie.get(key);
        if (oldValue !== undefined) {
            return error(409, `Key "${key}" already exists`, res);
        }
        await trie.insert(key, value);
        return success(201, { message: "Inserted", key, value }, res);
        trie.save();
    } catch (err) {
        console.error("Error during insertion:", err);
        return error(500, "Internal Server Error", res);
    }
};

const deleteHandler = (trie) => async (req, res) => {
    try {
        const { key } = req.body;
        if (!key) {
            return error(400, "Key is required", res);
        }
        const oldValue = await trie.get(key);
        if (oldValue === undefined) {
            return error(404, `Key "${key}" not found`, res);
        }
        await trie.delete(key);
        return success(200, { message: "Deleted successfully", key }, res);
    } catch (err) {
        console.error("Error during deletion:", err);
        return error(500, "Internal Server Error", res);
    }
};

const getHandler = (trie) => async (req, res) => {
    try {
        const { key } = req.body;
        if (!key) {
            return error(400, "Key is required", res);
        }
        const value = await trie.get(key);
        if (value === undefined) {
            return error(404, `Key "${key}" not found`, res);
        }
        return success(200, { key, value }, res);
    } catch (err) {
        console.error("Error during retrieval:", err);
        return error(500, "Internal Server Error" , err, res);
    }
};

const proofHandler = (trie) => async (req, res) => {
    try {
        const { key } = req.body;
        if (!key) {
            return error(400, "Key is required", res);
        }
        const value = await trie.get(key);
        if (value === undefined) {
            return error(404, `Key "${key}" not found`, res);
        }
        const proof = await trie.prove(key);
        return success(200, {
            key,
            value,
            proof: {
                json: proof.toJSON(),
                cbor: proof.toCBOR(),
                aiken: proof.toAiken()
            }
        }, res);
    } catch (err) {
        console.error("Error generating proof:", err);
        return error(500, "Internal Server Error", res);
    }
};

const rootHandler = (trie) => async (req, res) => {
    try {
        console.log("Getting root");
        const root = trie.hash;
        return success(200, { root }, res);
    } catch (err) {
        console.error("Error retrieving root:", err);
        return error(500, "Internal Server Error", err, res);
    }
}

async function startServer() {
    const app = express();
    const port = 3000;
    app.use(express.json());
    console.log(fs.existsSync(db_dir));
    const store = new Store(db_dir);
    const trie = fs.existsSync(db_dir)
        ? await Trie.load(store)
        : await new Trie(store);

    app.get('/root', rootHandler(trie));
    app.put('/value', handleInsert(trie));
    app.delete('/value', deleteHandler(trie));
    app.get('/value', getHandler(trie));
    app.get('/proof', proofHandler(trie));

    app.listen(port, () => {
        console.log(`Server running at http://localhost:${port}`);
    });
}

startServer();
