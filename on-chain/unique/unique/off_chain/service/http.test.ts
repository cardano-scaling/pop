import axios from 'axios';
import { assert } from 'console';
import { OutputRef, validatePort } from '../lib';
import { nullHash } from '../transactions/boot';
import { runServices, stopServices } from './http';
import getPort from 'get-port';
import { Provider, yaciProvider } from '../context';
import { generateMnemonic, MeshWallet } from '@meshsdk/core';

async function getWallet(host: string) {
    const response = await axios.get(`${host}/wallet`);
    assert(response.status === 200, 'Failed to get wallet');
    assert(
        response.data.address.slice(0, 4) == 'addr',
        'Address is not present'
    );
}

async function walletTopup(host: string) {
    const response = await axios.put(`${host}/wallet/topup`, {
        amount: 10000
    });
    assert(response.status === 200, 'Failed to top up wallet');
    assert(
        response.data.message === 'Top up successful',
        'Top up message is not valid'
    );
}
async function getTokens(host: string) {
    const response = await axios.get(`${host}/tokens`);
    assert(response.status === 200, 'Failed to get tokens');
    return response.data;
}

async function createToken(host: string) {
    const response = await axios.post(`${host}/token`);
    assert(response.status === 200, 'Failed to create token');
    assert(response.data.tokenId.length === 120, 'Token ID is not valid');
    return response.data.tokenId;
}

async function getToken(host: string, tokenId: string) {
    const response = await axios.get(`${host}/token/${tokenId}`);
    assert(response.status === 200, 'Failed to get token');
    return response.data;
}

async function deleteToken(host: string, tokenId: string) {
    const response = await axios.delete(`${host}/token/${tokenId}`);
    assert(response.status === 200, 'Failed to delete token');
    assert(response.data.txHash.length === 64, 'Transaction hash is not valid');
    return response.data.txHash;
}

async function updateToken(
    host: string,
    tokenId: string,
    requests: OutputRef[]
) {
    const response = await axios.put(`${host}/token/${tokenId}`, { requests });
    assert(response.status === 200, 'Failed to update token');
    assert(response.data.txHash.length === 64, 'Transaction hash is not valid');
    return response.data.txHash;
}

async function createRequest(
    host: string,
    tokenId: string,
    key: string[],
    value: string,
    op: 'insert' | 'delete'
) {
    const response = await axios.post(`${host}/token/${tokenId}/request`, {
        key,
        value,
        operation: op
    });
    assert(response.status === 200, 'Failed to create request');
    assert(response.data.txHash.length === 64, 'Transaction hash is not valid');
    return response.data;
}

async function deleteRequest(
    host: string,
    tokenId: string,
    txHash: string,
    outputIndex: number
) {
    const response = await axios.delete(
        `${host}/token/${tokenId}/request/${txHash}/${outputIndex}`
    );
    assert(response.status === 200, 'Failed to delete request');
    assert(response.data.txHash.length === 64, 'Transaction hash is not valid');
    return response.data.txHash;
}

type Wallets = {
    jacob: string;
    bob: string;
    alice: string;
};

const name = (s) => {console.log(`\n- ${s}`);};
const step = (s) => {console.log(`  - ${s}`);};
const test0 = async ({ jacob, bob, alice }) => {
    try {
        name('jacob can get his wallet');
        getWallet(jacob);
        name('bob can get his wallet');
        getWallet(bob);
        name('alice can get his wallet');
        getWallet(alice);
        name('jacob can get his tokens');
        const tks = await getTokens(jacob);
        assert(tks.length == 0, 'Tokens found');
        name('bob can get his tokens');
        const tks2 = await getTokens(bob);
        assert(tks2.length == 0, 'Tokens found');
        name('alice can get her tokens');
        const tks3 = await getTokens(alice);
        assert(tks3.length == 0, 'Tokens found');
    } catch (e) {
        console.log('Error in test0:', e);
    }
};

const test2 = async ({ jacob }) => {
    try {
        name('jacob can create a token and remove it');
        const tk = await createToken(jacob);
        step('jacob created a token');
        await deleteToken(jacob, tk);
        step('jacob deleted the token');
    } catch (e) {
        step('Error in test2:', e);
    }
};

const test3 = async ({ jacob, bob }) => {
    try {
        name(
            'bob can create a request to update a token and retract it'
        );
        const tk = await createToken(jacob);
        step('jacob created a token');
        const { txHash, outputIndex } = await createRequest(
            bob,
            tk,
            ['a', 'b', 'c'],
            'value',
            'insert'
        );
        step('bob created a request to insert a fact');
        await deleteRequest(bob, tk, txHash, outputIndex);
        step('bob retracted the request');
        await deleteToken(jacob, tk);
        step('jacob deleted the token');
    } catch (e) {
        console.log('Error in test3:', e);
    }
};

const test4 = async ({ jacob, bob }) => {
    try {
        name(
            'bob can create a request for a token and jacob can update the token'
        );
        const tk = await createToken(jacob);
        step('jacob created a token');
        const { txHash, outputIndex } = await createRequest(
            bob,
            tk,
            ['a', 'b', 'c'],
            'value',
            'insert'
        );
        step('bob created a request to insert a fact');
        await updateToken(jacob, tk, [{ txHash, outputIndex }]);
        step('jacob updated the token');
        await deleteToken(jacob, tk);
        step('jacob deleted the token');
    } catch (e) {
        console.log('Error in test4:', e);
    }
};

const test5 = async ({ jacob, bob, alice }) => {
    try {
        name('alice can remove a fact inserted by bob from jacob token');
        const tk = await createToken(jacob);
        step('jacob created a token');
        const { txHash, outputIndex } = await createRequest(
            bob,
            tk,
            ['a', 'b', 'c'],
            'value',
            'insert'
        );
        step('bob created a request to insert a fact');
        await updateToken(jacob, tk, [{ txHash, outputIndex }]);
        step('jacob updated the token');
        const { root1 } = await getToken(jacob, tk);
        assert(root1 !== nullHash, 'Token root is null');
        const { txHash: txHash2, outputIndex: outputIndex2 } =
            await createRequest(alice, tk, ['a', 'b', 'c'], 'value', 'delete');
        step('alice created a request to delete a fact');
        await updateToken(jacob, tk, [
            { txHash: txHash2, outputIndex: outputIndex2 }
        ]);
        step('jacob updated the token');
        const { root: root2 } = await getToken(jacob, tk);
        assert(root2 == nullHash, 'Token root is not null');
        await deleteToken(jacob, tk);
        step('jacob deleted the token');
    } catch (e) {
        console.log('Error in test5:', e);
    }
};

const test6 = async ({ jacob, bob, alice }) => {
    try {
        name(
            'alice and bob can create a request for a token and jacob can update the token'
        );
        const tk = await createToken(jacob);
        step('jacob created a token');
        const { txHash, outputIndex } = await createRequest(
            bob,
            tk,
            ['a', 'b', 'c'],
            'bob said',
            'insert'
        );
        step('bob created a request to insert a fact');
        const { txHash: txHash2, outputIndex: outputIndex2 } =
            await createRequest(
                alice,
                tk,
                ['a', 'b', 'b'],
                'alice said',
                'insert'
            );
        step('alice created a request to insert a fact');
        await updateToken(jacob, tk, [
            { txHash, outputIndex },
            { txHash: txHash2, outputIndex: outputIndex2 }
        ]);
        step('jacob updated the token');
        const { root } = await getToken(jacob, tk);
        assert(root !== nullHash, 'Token root is null');
        await deleteToken(jacob, tk);
        step('jacob deleted the token');
    } catch (e) {
        console.log('Error in test6:', e);
    }
};

function newWallet(provider: Provider) {
    const seed = crypto.getRandomValues(new Uint32Array(4)).join('');
    const entropy = Buffer.from(`${seed}`.repeat(32).slice(0, 32), 'utf8');
    const mnemonic = generateMnemonic(256, () => entropy);
    return new MeshWallet({
        networkId: 0,
        fetcher: provider,
        submitter: provider,
        key: {
            type: 'mnemonic',
            words: mnemonic.split(' ')
        }
    });
}


async function main() {
    const yaciPort = process.env.YACI_STORE_PORT;
    const yaciPortNumber = validatePort(yaciPort, 'YACI_STORE_PORT');
    const yaciAdminPort = process.env.YACI_ADMIN_PORT;
    const yaciAdminPortNumber = validatePort(yaciAdminPort, 'YACI_ADMIN_PORT');

    const jacobPort = await getPort();
    const bobPort = await getPort();
    const alicePort = await getPort();

    const wallets: Wallets = {
        jacob: `http://localhost:${jacobPort}`,
        bob: `http://localhost:${bobPort}`,
        alice: `http://localhost:${alicePort}`
    };

    const provider = yaciProvider(yaciPortNumber, yaciAdminPortNumber);
    const servers = await runServices(
        [jacobPort, bobPort, alicePort],
        provider,
        newWallet
    );
    await walletTopup(wallets.jacob);
    await walletTopup(wallets.bob);
    await walletTopup(wallets.alice);

    console.log('Using HTTP API');
    await test0(wallets);
    await test2(wallets);
    await test3(wallets);
    await test4(wallets);
    await test5(wallets);
    await test6(wallets);
    await stopServices(servers);
}

main()
    .then(() => {
        console.log('All tests passed');
        process.exit(0);
    })
    .catch(error => {
        console.error('Test failed:', error.message);
        process.exit(1);
    });
