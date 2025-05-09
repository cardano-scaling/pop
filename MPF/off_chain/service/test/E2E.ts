import { validatePort } from '../../lib';
import { runServices, stopServices } from '../http';
import getPort from 'get-port';
import { Provider, yaciProvider } from '../../context';
import { generateMnemonic, MeshWallet } from '@meshsdk/core';
import { walletTopup } from './E2E/client';
import {
    canAccessWallets,
    cannotDeleteAnotherUsersToken,
    canRetractRequest,
    createTokenAndDelete as canCreateTokenAndDelete,
    Runner,
    tokensAreEmpty as canRetrieveTokens,
    Wallets,
    cannotRetractAnotherUsersRequest,
    cannotUpdateATokenWithNoRequests,
    canInspectRequestsForAToken,
    canUpdateAToken,
    cannotUpdateAnotherUsersToken,
    canDeleteFacts,
    canBatchUpdate,
    insertCommutes,
    deleteCommutes
} from './E2E/scenarios';
import { catchFailure } from './E2E/lib';

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

    const charliePort = await getPort();
    const bobPort = await getPort();
    const alicePort = await getPort();

    const wallets: Wallets = {
        charlie: `http://localhost:${charliePort}`,
        bob: `http://localhost:${bobPort}`,
        alice: `http://localhost:${alicePort}`
    };
    const yaciAdminHost = `http://localhost:${yaciAdminPortNumber}`;
    const yaciStoreHost = `http://localhost:${yaciPortNumber}`;

    const provider = yaciProvider(yaciStoreHost, yaciAdminHost);
    const servers = await runServices(
        [charliePort, bobPort, alicePort],
        provider,
        newWallet
    );
    await walletTopup(wallets.charlie);
    await walletTopup(wallets.bob);
    await walletTopup(wallets.alice);

    let failures: { error: Error; name: string }[] = [];

    const runner: Runner = {
        run: async (fn: () => Promise<void>, name: string) => {
            console.log(` - ${name}`);
            await catchFailure(fn, name, failures);
        },
        log: async (s: string) => {
            console.log(`  - ${s}`);
        },
        wallets
    };
    console.log('- using HTTP API');
    await canAccessWallets(runner);
    await canRetrieveTokens(runner);
    await canCreateTokenAndDelete(runner);
    await cannotDeleteAnotherUsersToken(runner);
    await canRetractRequest(runner);
    await cannotRetractAnotherUsersRequest(runner);
    await cannotUpdateATokenWithNoRequests(runner);
    await canInspectRequestsForAToken(runner);
    await canUpdateAToken(runner);
    await cannotUpdateAnotherUsersToken(runner);
    await canDeleteFacts(runner);
    await canBatchUpdate(runner);
    await insertCommutes(runner);
    await deleteCommutes(runner);
    await stopServices(servers);
    if (failures.length > 0) {
        console.log('Failures:');
        failures.forEach(({ error, name }) => {
            console.log(`  - ${error.message} in ${name}`);
        });
        throw new Error('Some tests failed');
    }
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
